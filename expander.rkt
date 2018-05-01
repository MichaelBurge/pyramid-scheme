#lang typed/racket

(require typed/racket/unsafe)
(require (submod "types.rkt" common))
(require (submod "types.rkt" ast))
(require (submod "types.rkt" abstract-machine))
(require (submod "types.rkt" evm-assembly))
(require (submod "types.rkt" pyramidc))
(require "globals.rkt")
(require (submod "typed.rkt" binaryio))

(require/typed racket/pretty
  [ pretty-print (-> Any Void)])

(unsafe-provide (all-from-out 'typed))

(module* macros racket
  (require (for-syntax (submod "." "..")))
  (require (submod "." ".."))
  (provide (all-defined-out)
           (all-from-out (submod "types.rkt" ast))
           (all-from-out (submod "." "..")))

  (require (submod "types.rkt" ast))

  (require (for-syntax racket/match))

  (define-syntax (quasiquote-pyramid stx)
    (define (loop stx)
      (syntax-case stx (unquote unquote-splicing quasiquote)
        [(unquote y)           #'(unsyntax (shrink-pyramid y))]
        [(unquote-splicing ys) #'(unsyntax-splicing (map shrink-pyramid ys))]
        [(quasiquote y)        #`(expand-pyramid (#,#'quasisyntax #,(loop #'y)))]
        [(xs ...) #`(#,@(map loop (syntax-e #'(xs ...))))]
        [x #'x]
        ))
    (syntax-case stx ()
      [(_ exp) (loop #'exp)]))
  )

(module primitive-ast typed/racket
  (require (submod "types.rkt" ast))
  (require "globals.rkt")
  (provide (all-defined-out))

  (: sequence->exp (-> Pyramids Pyramid))
  (define (sequence->exp seq)
    (match seq
      (`() (pyr-begin '()))
      ((list x) x)
      (xs (pyr-begin xs))))
  )

(module expanders racket
  (require syntax/parse)
  (require (submod "types.rkt" ast))
  (require (submod "types.rkt" abstract-machine))
  (require (submod "types.rkt" evm-assembly))
  (require (submod ".." primitive-ast))
  (require (submod "typed.rkt" binaryio))
  (require "utils.rkt")
  (require (submod "utils.rkt" syntax-parse))
  (require "globals.rkt")

  (provide (all-defined-out))

  (define-syntax (box-literals? stx)
    (syntax-case stx ()
      [_ #'(expander-options-box-literals? (*expander-options*))]))

  (define (get-output-expander-options stx)
    (define v (syntax-property stx STXPROP-OUT-OPTIONS))
    (if v
        v
        (*expander-options*)))

  (define (set-output-expander-options stx #:opts [opts (*expander-options*)])
    (syntax-property stx STXPROP-OUT-OPTIONS opts))

  (define-literal-set pyramid-keyword
    #:datum-literals (quote box unbox define λ lambda reg const boxed-const op stack label evm
                            assign test branch goto save restore perform
                            push byte bytes set! if begin asm #%dot)
    ())

  (define pyramid-keyword? (literal-set->predicate pyramid-keyword))

  (define-syntax-class pyramid-identifier
    #:literal-sets (pyramid-keyword)
    [pattern (~and x:identifier (~fail #:when (pyramid-keyword? #'x)))]
  )

  ; See Note[1] on the design of the constant literals.
  ; TODO: Attach "boxed?" annotations during read of the initial parse tree.
  (define-syntax-class pyramid-constant
    #:datum-literals (quote unbox box)
    #:attributes (ast)
    [pattern x:boolean                  #:attr ast (pyr-const (datum x) #f)]
    [pattern x:integer                  #:attr ast (pyr-const (datum x) box-literals?)]
    [pattern x:string                   #:attr ast (pyr-const (datum x) box-literals?)]
    [pattern x:char                     #:attr ast (pyr-const (datum x) box-literals?)]
    [pattern #(xs:exact-integer ...)    #:attr ast (pyr-const (syntax->datum this-syntax) box-literals?)]
    [pattern (quote x:identifier)       #:attr ast (pyr-const (datum x) box-literals?)]
    [pattern (unbox ~! x:pyramid-constant) #:attr ast (struct-copy pyr-const (attribute x.ast) [ boxed? #f ])]
    [pattern (box   ~! x:pyramid-constant) #:attr ast (struct-copy pyr-const (attribute x.ast) [ boxed? #t ])]
    )

  (define-syntax-class pyramid-definition
    #:datum-literals (define)
    #:attributes (ast)
    [pattern (define name:pyramid-identifier ~! value:pyramid)
             #:attr ast (pyr-definition (datum name) (attribute value.ast))]
    [pattern (define ~! (name:pyramid-identifier vars:pyramid-identifier ...) body:pyramid ...)
             #:attr ast (pyr-definition (datum name)
                                        (pyr-lambda (map syntax->datum (attribute vars))
                                                    (sequence->exp (attribute body.ast))))]
    )

  (define-syntax-class pyramid-lambda
    #:datum-literals (λ lambda)
    #:attributes (ast)
    [pattern ((~or* λ lambda) body:pyramid) #:attr ast (pyr-lambda '() (attribute body.ast))]
    [pattern ((~or* λ lambda) parameter:pyramid-identifier body:pyramid ...)
             #:attr ast (pyr-lambda (list (datum parameter))
                                    (sequence->exp (attribute body.ast)))]
    [pattern ((~or* λ lambda) (parameters:pyramid-identifier ...) body:pyramid ...)
             #:attr ast (pyr-lambda (map syntax->datum (attribute parameters))
                                    (sequence->exp (attribute body.ast)))]
    [pattern ((~or* λ lambda) body:pyramid ...)
             #:attr ast (pyr-lambda '() (sequence->exp (attribute body.ast)))]
    )

  (define-syntax-class register
    #:datum-literals (quote env proc continue argl val stack-size)
    #:attributes (name)
    [pattern (quote env)        #:attr name 'env]
    [pattern (quote proc)       #:attr name 'proc]
    [pattern (quote continue)   #:attr name 'continue]
    [pattern (quote argl)       #:attr name 'argl]
    [pattern (quote val)        #:attr name 'val]
    [pattern (quote stack-size) #:attr name 'stack-size]
    )

  (define-syntax-class register-value
    #:datum-literals (quote)
    #:attributes (value)
    [pattern (quote x:identifier) #:attr value (datum x)]
    [pattern (~or* x:boolean x:integer x:string) #:attr value (datum x)]
    [pattern (quote (~or* (xs:integer ...) (xs:identifier ...) (xs:string ...))) #:attr value (datum xs)]
    [pattern (~and x #(_:integer ...)) #:attr value (datum x)]
    )

  (define-syntax-class machine-expression
    #:datum-literals (reg const boxed-const op stack label evm)
    #:attributes (mexpr)
    [pattern (reg ~! x:register) #:attr mexpr (reg (attribute x.name))]
    [pattern (const ~! val:register-value) #:attr mexpr (const (attribute val.value))]
    [pattern (boxed-const ~! val:register-value) #:attr mexpr (boxed-const (attribute val.value))]
    [pattern (op ~! (quote name:identifier) args:machine-expression ...) #:attr mexpr (op (datum name) (attribute args.mexpr))]
    [pattern stack #:attr mexpr stack]
    [pattern (label ~! (quote name:identifier)) #:attr mexpr (label (datum name))]
    [pattern (evm ~! is:pyramid-evm ...) #:attr mexpr (evm (attribute is.evm-instruction))]
    )

  (define-syntax-class pyramid-label-definition
    #:datum-literals (quote label)
    #:attributes (label-obj)
    [pattern (label ~! (quote name:identifier)
                       (~optional os:integer #:defaults ([os #'0]))
                       (~optional virtual?:boolean #:defaults ([virtual? #'#f])))
             #:attr label-obj (label-definition (datum name) (datum os) (datum virtual?))]
    )

  (define-syntax-class pyramid-abstract-instruction
    #:datum-literals (assign test branch goto save restore perform evm quote)
    #:attributes (instruction)
    [pattern label:pyramid-label-definition            #:attr instruction (attribute label.label-obj)]
    [pattern (assign  ~! reg:register value:machine-expression) #:attr instruction (assign (attribute reg.name) (attribute value.mexpr))]
    [pattern (test    ~! condition:machine-expression) #:attr instruction (test (attribute condition.mexpr))]
    [pattern (branch  ~! dest:machine-expression)      #:attr instruction (branch (attribute dest.mexpr))]
    [pattern (goto    ~! dest:machine-expression)      #:attr instruction (goto (attribute dest.mexpr))]
    [pattern (save    ~! e:machine-expression)         #:attr instruction (save (attribute e.mexpr))]
    [pattern (restore ~! reg:register)                 #:attr instruction (restore (attribute reg.name))]
    [pattern (perform ~! e:machine-expression)         #:attr instruction (perform (attribute e.mexpr))]
    [pattern (evm     ~! is:pyramid-evm ...)           #:attr instruction (evm (attribute is.evm-instruction))]
    )

  (define-syntax-class byte
    [pattern (~and val:exact-integer (~fail #:unless (and (<= 0 (datum val)) (<= (datum val) 255))))]
    )

  (define-syntax-class pyramid-evm
    #:datum-literals (push op byte bytes label quote shrink)
    #:attributes (evm-instruction)
    [pattern (push 'shrink (label (quote name:identifier))) #:attr evm-instruction (evm-push 'shrink (label (datum name)))]
    [pattern (push 'shrink val:integer)        #:attr evm-instruction (evm-push 'shrink (datum val))]
    [pattern (push size:exact-positive-integer val:integer)      #:attr evm-instruction (evm-push (datum size) (datum val))]
    [pattern (~or* (quote name:identifier)
                   (op (quote name:identifier))) #:attr evm-instruction (evm-op (datum name))]
    [pattern (byte ~! x:exact-positive-integer)   #:attr evm-instruction (evm-bytes (bytes (datum x)))]
    [pattern (bytes ~! size:exact-positive-integer val:exact-nonnegative-integer)
             #:attr evm-instruction (evm-bytes (integer->bytes (datum val) (datum size) #f))]
    [pattern lbl:pyramid-label-definition      #:attr evm-instruction (attribute lbl.label-obj)]
    )

  (define-syntax-class pyramid-macro-definition
    #:datum-literals (define-syntax)
    #:attributes (ast)
    [pattern (define-syntax name:pyramid-identifier body:expr ...)
             #:attr ast (pyr-macro-definition (datum name)
                                              '()
                                              #`(begin #,@(attribute body))
                                              (get-output-expander-options this-syntax))]
    [pattern (define-syntax (name:pyramid-identifier . params) body:expr ...)
             #:attr ast (pyr-macro-definition (datum name)
                                              (attribute params)
                                              #`(begin #,@(attribute body))
                                              (get-output-expander-options this-syntax))]
    )

  (define-syntax-class pyramid-macro-application
    #:attributes (ast)
    [pattern ((~and (name:identifier (~fail #:unless (defined-macro? (datum name)))))
              args:expr ...)
             #:attr ast (pyr-macro-application (datum name) (set-output-expander-options this-syntax))]
    [pattern (name:pyramid-identifier . args:expr)
             ; NOTE: this-syntax could be a "syntax pair": https://docs.racket-lang.org/reference/stxops.html#%28tech._syntax._pair%29
             ; We need to set-output-expander-options in case there are recursive macro calls; it's not possible to call this
             ; on a syntax pair, but those cases only occur under quoted forms where there are no macros.
             #:attr ast (pyr-unknown-application (datum name)
                                                 (if (syntax? this-syntax)
                                                     (set-output-expander-options this-syntax)
                                                     this-syntax))]
    )

  (define-syntax-class pyramid-quoted
    #:datum-literals (quote)
    #:attributes (ast)
    [pattern (quote ~! x:pyramid) #:attr ast (pyr-quoted (attribute x.ast))]
    )

  (define-syntax-class pyramid-application
    #:attributes (ast)
    [pattern (f:pyramid args:pyramid ...)
             #:attr ast (pyr-application (attribute f.ast) (attribute args.ast) #f)]
    [pattern (~or* (#%dot (f:pyramid args:pyramid ...) rest:pyramid)
                   (f:pyramid args:pyramid ... . rest:pyramid))
             #:attr ast (pyr-application (attribute f.ast) (attribute args.ast) (attribute rest.ast))]
    )

  (define-syntax-class pyramid-variable
    #:attributes (ast)
    [pattern x:pyramid-identifier #:attr ast (pyr-variable (datum x))]
    )

  (define-syntax-class pyramid-assignment
    #:datum-literals (set!)
    #:attributes (ast)
    [pattern (set! ~! name:identifier x:pyramid) #:attr ast (pyr-assign (datum name) (attribute x.ast))]
    )

  (define-syntax-class pyramid-if
    #:datum-literals (if)
    #:attributes (ast)
    [pattern (if ~! p:pyramid t:pyramid f:pyramid) #:attr ast (pyr-if (attribute p.ast) (attribute t.ast) (attribute f.ast))]
    )

  (define-syntax-class pyramid-begin
    #:datum-literals (begin)
    [pattern (begin ~! body:pyramid ...)     #:attr ast (pyr-begin (attribute body.ast))]
    )

  (define-syntax-class pyramid-asm
    #:datum-literals (asm)
    [pattern (asm ~! ops:pyramid-abstract-instruction ...)  #:attr ast (pyr-asm (attribute ops.instruction))]
    )

  (define-syntax-class pyramid
    #:datum-literals (set! if begin asm)
    #:attributes (ast)
    [pattern :pyramid-constant]
    [pattern :pyramid-variable]
    [pattern :pyramid-quoted]
    [pattern :pyramid-assignment]
    [pattern :pyramid-definition]
    [pattern :pyramid-lambda]
    [pattern :pyramid-if]
    [pattern :pyramid-begin]
    [pattern :pyramid-asm]
    [pattern :pyramid-macro-definition]
    [pattern :pyramid-macro-application]
    [pattern :pyramid-application]
    )

  ; (: expand-pyramid (-> PyramidQ Pyramid))x
  (define (expand-pyramid x #:options [ options (*expander-options*)])
    (parameterize ([ *expander-options* options])
      (syntax-parse x
        [pyr:pyramid (attribute pyr.ast)]
        )))

  ; (: expand-asm (-> PyramidQ Instruction))
  (define (expand-asm x)
    (syntax-parse x
      [asm:pyramid-abstract-instruction (attribute asm.instruction)]))

  ;; (define (expand-pyramid-datum x)
  ;;   (syntax->datum (expand-pyramid x)))



  ;(: defined-macro? (-> Any Boolean : #:+ Symbol))
  (define (defined-macro? name)
    (and (symbol? name)
         (namespace-contains? (*available-macros*) name)))

  (define (unknown->application x)
    (destruct pyr-unknown-application x)
    (parameterize ([ *expander-options* (or (syntax-property x-app-syntax STXPROP-OUT-OPTIONS)
                                            (error "unknown->application: Syntax not marked with expander options" x))])
      (syntax-parse x-app-syntax
        [ app:pyramid-application (attribute app.ast)]
        )))
  )

(module shrinkers typed/racket/no-check
  (require typed/racket/unsafe)
  (require (submod "types.rkt" ast))
  (require (submod "types.rkt" abstract-machine))
  (require (submod "types.rkt" evm-assembly))
  (require (submod "typed.rkt" binaryio))
  (require "globals.rkt")
  (require (submod "utils.rkt" unsafe))
  (require (submod ".." expanders))

  (unsafe-provide (all-defined-out))

  (: pyramid->datum (-> Pyramid Sexp))
  (define (pyramid->datum x)
    (syntax->datum (shrink-pyramid x)))

  (: value->datum (-> value [#:env? Boolean] Sexp))
  (define (value->datum x #:env? [env? (verbose? VERBOSITY-MEDIUM)])
    (syntax->datum (shrink-value x #:env? env?)))

  (: evm->datum (-> EthInstruction Sexp))
  (define (evm->datum x)
    (syntax->datum (shrink-evm x)))

  (: asm->datum (-> Instruction Sexp))
  (define (asm->datum x)
    (syntax->datum (shrink-asm x)))

  (: shrink-pyramid (-> Pyramid PyramidQ))
  (define (shrink-pyramid x)
    (match x
      ; TODO: Add a "plain" syntax property
      [(struct pyr-const (v #f))                           #`(unbox #,(if (symbol? v) #`(quote #,v) v))]
      [(struct pyr-const (v #t))                           #`(box   #,(if (symbol? v) #`(quote #,v) v))]
      [(struct pyr-variable (v))                           #`#,v]
      [(struct pyr-quoted (exp))                           #`(quote #,(shrink-pyramid exp))]
      [(struct pyr-assign (name value))                    #`(set! #,name #,(shrink-pyramid value))]
      [(struct pyr-definition (name body))                 #`(define #,name #,(shrink-pyramid body))]
      [(struct pyr-lambda (vars body))                     #`(λ #,vars #,(shrink-pyramid body))]
      [(struct pyr-if (pred cons alt))                     #`(if #,(shrink-pyramid pred) #,(shrink-pyramid cons) #,(shrink-pyramid alt))]
      [(struct pyr-begin (actions))                        #`(begin #,@(map shrink-pyramid actions))]
      [(struct pyr-macro-definition (name args body opts)) (syntax-property #`(define-syntax (#,name #,@args) #,body)
                                                                            STXPROP-OUT-OPTIONS opts)]
      [(struct pyr-macro-application (name stx))           stx]
      [(struct pyr-asm (ops))                              #`(asm #,@(map shrink-asm ops))]
      [(struct pyr-application (op xs #f))                 #`(#,(shrink-pyramid op) #,@(map shrink-pyramid xs))]
      [(struct pyr-application (op xs dot))                #`(#%dot (#,(shrink-pyramid op) #,@(map shrink-pyramid xs)) #,(shrink-pyramid dot))]
      [(struct pyr-unknown-application (op stx))           stx]
      ))

  (: shrink-evm (-> EthInstruction PyramidQ))
  (define (shrink-evm asm)
    (match asm
      [(struct evm-push ('shrink (struct label (name)))) #`(push 'shrink (label (quote #,name)))]
      [(struct evm-push ('shrink value))       #`(push 'shrink #,value)]
      [(struct evm-push (size value))          #`(push #,size #,value)]
      [(struct evm-op   (sym))                 #`(op (quote #,sym))]
      [(struct evm-bytes (bs)) (match (bytes-length bs)
                                 [ 1           #`(byte #,(first (bytes->list bs)))]
                                 [ n           #`(bytes #,n #,(bytes->integer bs #f))])]
      ;[(struct pyr-asm-cg (exp))                   exp]
      [(struct label-definition (name 0  #f))      #`(label (quote #,name))]
      [(struct label-definition (name os #f))      #`(label (quote #,name) #,os)]
      [(struct label-definition (name os virtual)) #`(label (quote #,name) #,os #,virtual)]
      [_ (error "shrink-evm: Unknown syntax" asm)]))

  (: shrink-asm (-> Instruction PyramidQ))
  (define (shrink-asm i)
    (match i
      [(struct label-definition (name 0        #f)) #`(label   (quote #,name)                      )]
      [(struct label-definition (name os       #f)) #`(label   (quote #,name) #,os                 )]
      [(struct label-definition (name os virtual?)) #`(label   (quote #,name) #,os #,virtual?      )]
      [(struct assign           (name       value)) #`(assign  (quote #,name) #,(shrink-mexpr value))]
      [(struct test             (condition       )) #`(test    #,(shrink-mexpr condition)          )]
      [(struct branch           (dest            )) #`(branch  #,(shrink-mexpr dest)               )]
      [(struct goto             (dest            )) #`(goto    #,(shrink-mexpr dest)               )]
      [(struct save             (exp             )) #`(save    #,(shrink-mexpr exp)                )]
      [(struct restore          (name            )) #`(restore (quote #,name)                      )]
      [(struct perform          (op              )) #`(perform #,(shrink-mexpr op)                 )]
      [(struct evm              (is              )) #`(evm     #,@(map shrink-evm is)              )]
      [_ (error "shrink-abstract: Unknown syntax" i)]
      ))

  (: shrink-mexpr (-> MExpr PyramidQ))
  (define (shrink-mexpr e)
    (define (ensure-quoted x)
      (if (symbol? x) `(quote ,x) x))
    (match e
      [(struct reg         (name     )) #`(reg (quote #,name) )]
      [(struct const       (value    )) #`(const #,(ensure-quoted value))]
      [(struct boxed-const (value    )) #`(boxed-const #,(ensure-quoted value))]
      [(struct op          (name args)) #`(op (quote #,name) #,@(map shrink-mexpr args))]
      [(struct %stack      (         )) #'stack               ]
      [(struct label       (name))      #`(label (quote #,name))]
      [(struct evm         (is))        #`(evm #,@(map (λ (x) (list 'quote x))
                                                       ((inst map Symbol evm-op) evm-op-name (cast is (Listof evm-op)))))]
      [_ (error "shrink-mexpr: Unknown syntax" e)]
      ))

  (: shrink-value (-> value [#:env? Boolean] PyramidQ))
  (define (shrink-value e #:env? [env? (verbose? VERBOSITY-MEDIUM)])
    (match e
      [(struct v-fixnum (v _)) #`(box #,v)]
      [(struct v-symbol (v)) #`(box #,v)]
      [(struct v-char   (v)) #`(box #,v)]
      [(struct v-compiled-procedure (lbl env)) #`(λ #,(shrink-value lbl))]
      [(struct v-primitive-procedure (lbl))    #`(λ* #,(shrink-value lbl))]
      [(struct v-pair (left right)) #`(#,(shrink-value left) . #,(shrink-value right))]
      [(struct v-vector (vs)) #`#,(vector-map shrink-value vs)]
      [(struct v-null ()) #'()]
      [(struct v-continuation (cont env stack)) #`(λ-> #,(shrink-value cont) ,stack)]
      [(struct v-frame (mappings)) #`(quote #,mappings)]
      [(struct v-environment (frames)) #`(env #,@(if env? (map shrink-value frames) null))]
      [(struct v-bytes (ptr)) #`(bytes #,ptr)]
      [(struct label (name)) #`(quote #,(label-name e))]
      [(? v-unboxed? _) #`(quote #,e)]
      ))
  )
;; (require 'expanders)
;; (require 'shrinkers)

(module typed typed/racket
  (require typed/racket/unsafe)
  (require (submod "types.rkt" ast))
  (require (submod "types.rkt" abstract-machine))
  (require (submod "types.rkt" evm-assembly))
  (unsafe-provide expand-pyramid
                  shrink-pyramid
                  shrink-value
                  shrink-asm
                  shrink-evm
                  pyramid->datum
                  value->datum
                  asm->datum
                  evm->datum
                  unknown->application
                  )
  (unsafe-require/typed (submod ".." expanders)
                        [ expand-pyramid (case-> (-> PyramidQ Pyramid)
                                                 (-> (Syntaxof Any) Pyramid))]
                        [ unknown->application (-> pyr-unknown-application pyr-application)]
                        )
  (unsafe-require/typed (submod ".." shrinkers)
                        [ shrink-pyramid (-> Pyramid PyramidQ)]
                        [ shrink-value (-> value [#:env? Boolean] PyramidQ)]
                        [ shrink-asm (-> Instruction PyramidQ)]
                        [ shrink-evm (-> EthInstruction PyramidQ)]
                        [ pyramid->datum (-> Pyramid Sexp)]
                        [ value->datum (-> value [#:env? Boolean] Sexp)]
                        [ asm->datum (-> Instruction Sexp)]
                        [ evm->datum (-> EthInstruction Sexp)]
                        )
  )

(require 'typed)

#| Note[1] - Design of Constant Literals
The pyramid and pyramid/core languages wanted to assume integers were boxed and unboxed respectively.

This caused some design issues, which are summarized below along with the decision.

Constant literals have 3 attributes
1. Semantically boxed or unboxed?
2. Syntactically boxed, unboxed, or plain?
3. The interpretation of plain integers at its original source position.

Requirements
1. Macros should be able to detect whether the source was originally a boxed, unboxed, or plain integer. I'd be willing to compromise on "unboxed = plain" being indistinguishable.
2. pyramid/core macros should be usable without any changes in pyramid source.

Potential Implementations
* Disambiguate all integers into boxed or unboxed. Problem: Macros expect the plain form.
* Use the current boxing status to decide which. Problem: Macros might want to match all three forms.
* Always assume plain integers are boxed. Problem: pyramid/core macros expect unboxed values
* Always assume plain integers are unboxed. Problem: pyramid macros expect boxed values
* Add a "plain-boxed" or "plain-unboxed" boxing type. Problem: The original source text is ambiguous without the associated parser.
* The pyramid language should convert inputs like (box 5) to 5. Problem: (box 5) could be returned by a pyramid/core macro, rather than source. Macros lose generality. Workaround: "Full" macros could be written in pyramid/core.
* Macros should all collapse (box 5) or (unbox 5) into the same equivalence class. Problem: Macros become awkward to write.
* Change expand-pyramid to operate on syntax rather than S-expressions. Attach syntax properties with all necessary information. Problem: Some work rewriting it to use syntax/parse Benefit: Handles types, optimization hints, etc.; generalize to other languages

Decision
* The pyr-const will have two booleans: boxed?, plain?
* expand-pyramid and shrink-pyramid will be changed to operate on syntax expressions.
* The defmacro core form will be replaced with define-syntax
* The compiler uses boxed? only to emit code
* Macros receive either a plain("5"), boxed("(box 5)"), or unboxed("(unbox 5)") depending on plain?.
|#
