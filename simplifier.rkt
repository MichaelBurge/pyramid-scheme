#lang typed/racket

(require "types.rkt")
(require "ast.rkt")
(require "io.rkt")
(require "expander.rkt")
(require "utils.rkt")
(require "globals.rkt")
(require racket/pretty)

(require typed/racket/unsafe)

(unsafe-require/typed racket/set
   [ set-add! (All (A) (-> (Setof A) A Void))]
   [ mutable-set (All (A) (-> (Setof A)))])

(define *pass-number* 0)

(provide simplify
         simplify-macros
         (all-defined-out))

(: simplify Pass)
(define (simplify prog)
  (set! prog (fixpass (λ ([ prog : Pyramid])
                          (set! prog (fixpass pass-expand-macros prog))
                          (*assume-macros-complete?* #t)
                          (set! prog (fixpass pass-determine-unknown-applications prog))
                          prog)
                      prog))
  (set! prog (pass-inline-simple-definitions prog))
  (set! prog (fixpass pass-remove-unused-definitions prog))
  (set! prog (pass-collapse-nested-begins prog))
  ;(pretty-print prog)
  (set! prog (pass-error-on-undefined-variables prog))
  ;(pretty-print prog)
  (set! prog (pass-remove-empty-asms prog))
  (set! prog (pass-collapse-nested-begins prog))
  ;(pretty-print prog)
  prog
  )

(define-syntax-rule (define-pass (name prog) body ...)
  (define (name prog)
    (define (pass) body ...)
    (define new-x (pass))
    (verbose-section
     (format "AST pass '~a" (syntax->datum #'name))
     VERBOSITY-HIGH
     (pretty-print (pyramid->datum new-x)))
    (set! *pass-number* (+ *pass-number* 1))
    new-x))

(: pass-expand-macros Pass)
(define-pass (pass-expand-macros prog)
  (define prog2
    (transform-ast-descendants-on prog pyr-macro-definition?
                                  (λ ([ x : pyr-macro-definition ])
                                    (begin
                                      (install-macro-exp! x)
                                      (pyr-begin '())))))
  (define prog3 (fixpass
                 (λ (x)
                   (transform-ast-descendants-on x pyr-macro-application? expand-macro))
                 prog2))
  (define prog4 (pass-determine-unknown-applications prog3))
  prog4
  )

(: simplify-macros Pass)
(define simplify-macros (λ ([ prog : Pyramid ]) (fixpass pass-expand-macros prog)))

(: pass-remove-unused-definitions Pass)
(define-pass (pass-remove-unused-definitions prog)
  (let ([ unuseds (set-subtract (defined-vars prog) (used-vars prog))])
    (remove-definitions prog unuseds)))

(: pass-error-on-undefined-variables Pass)
(define-pass (pass-error-on-undefined-variables prog)
  (let ([ undefineds (set-subtract (used-vars prog) (defined-vars prog))])
    (if (set-empty? undefineds)
        prog
        (begin
          (print-ast prog)
          (error "Undefined variables" undefineds)))))

(: pass-collapse-nested-begins Pass)
(define-pass (pass-collapse-nested-begins prog)
  (transform-ast-descendants-on
   prog
   pyr-begin?
   (λ ([ x : pyr-begin ])
     (sequence->exp
      (apply append
             (map (λ ([y : Pyramid])
                    (if (pyr-begin? y)
                        (pyr-begin-body y)
                        (list y)))
                  (pyr-begin-body x)))))))

(: pass-inline-simple-definitions Pass)
(define-pass (pass-inline-simple-definitions prog)
  (: nonsimple-definitions (Setof Symbol))
  (define nonsimple-definitions (mutable-set))
  (: simple-definitions (HashTable Symbol Pyramid))
  (define simple-definitions (make-hash))
  (: simple-definition? (-> Pyramid Boolean : #:+ pyr-definition))
  (define (simple-definition? x)
    (and (pyr-definition? x)
         (not (set-member? nonsimple-definitions (pyr-definition-name x)))
         (match (pyr-definition-body x)
           [(? pyr-const?)    #t]
           [(? pyr-variable?) #t]
           [(? pyr-quoted?)   #t]
           [_                 #f])))
  ; Mutated variables can't be inlined.
  (let ([ prog (transform-ast-descendants-on
                prog pyr-assign?
                (λ ([ x : pyr-assign])
                  (begin
                    (set-add! nonsimple-definitions (pyr-assign-name x))
                    x)))])
    ; Remove simple definitions
    (let ([ prog (transform-ast-descendants-on
                  prog simple-definition?
                  (λ ([ x : pyr-definition])
                    (begin
                      (hash-set! simple-definitions
                                 (pyr-definition-name x)
                                 (pyr-definition-body x))
                      (pyr-begin '()))))])
      (let ([ prog (transform-ast-descendants-on
                    prog pyr-variable?
                    (λ ([x : pyr-variable])
                      (let ([ var (pyr-variable-name x)])
                        (if (hash-has-key? simple-definitions var)
                            (hash-ref simple-definitions var)
                            x))))])
        prog))))

(: *fixpass-num-iterations* (Parameterof Integer))
(define *fixpass-num-iterations* (make-parameter 1000))

(: fixpass (-> Pass Pyramid Pyramid))
(define (fixpass pass prog)
  (define n (*fixpass-num-iterations*))
  (if (< n 0)
      (error "fixpass: Maximum iterations reached reducing program" prog)
      (parameterize ([ *fixpass-num-iterations* (- n 1)])
        (let ([ newprog (pass prog) ])
          (if (equal? prog newprog)
              prog
              (fixpass pass newprog))))))

(: remove-definitions (-> Pyramid (Setof VariableName) Pyramid))
(define (remove-definitions prog vars)
  (: transform Pass)
  (define (transform x)
    (if (and (pyr-definition? x)
             (set-member? vars (pyr-definition-name x)))
        (pyr-begin '())
        x))
  (transform-ast-descendants prog transform))

; assume-macros-complete? means "Are all possible macros defined?".
; After macro expansion, every unknown application can default to a function application.
; But during macro expansion, a macro could be defined later even if it isn't now.
(: pass-determine-unknown-applications Pass)
(define-pass (pass-determine-unknown-applications ast)
  (transform-ast-descendants-on ast pyr-unknown-application?
                                (λ ([ x : pyr-unknown-application ])
                                  (: x-name VariableName)
                                  (destruct pyr-unknown-application x)
                                  (if (macro? x-name)
                                      (pyr-macro-application x-name x-app-syntax)
                                      (if (*assume-macros-complete?*)
                                          (unknown->application x)
                                          x)))))

(: pass-remove-empty-asms Pass)
(define-pass (pass-remove-empty-asms ast)
  (transform-ast-descendants-on ast pyr-asm?
                                (λ ([ x : pyr-asm ])
                                  (if (null? (pyr-asm-insts x))
                                      (pyr-begin (list))
                                      x))))

(: defined-vars (-> Pyramid (Setof VariableName)))
(define (defined-vars prog)
  (apply set (append (map pyr-definition-name (all-definitions prog))
                     (apply append (map pyr-lambda-names (all-lambdas prog))))))

(: used-vars (-> Pyramid (Setof VariableName)))
(define (used-vars prog)
  (let* ([ vars : pyr-variables (all-variables prog)]
         [ names : VariableNames ((inst map VariableName pyr-variable) pyr-variable-name vars)]
         [ assign-vars (all-assigns prog)]
         [ assign-var-names (map pyr-assign-name assign-vars)]
         )
    (set-union (apply set names)
               (apply set assign-var-names))))
