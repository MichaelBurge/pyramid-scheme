#lang typed/racket

(require typed/racket/unsafe)
(require (submod "types.rkt" ast))
(require "globals.rkt")
(require (submod "expander.rkt" primitive-ast))
(require "utils.rkt")

(unsafe-provide lookup-macro
                expand-macro
                install-macro!
                install-macro-exp!)

(provide (all-defined-out)
         (all-from-out (submod "types.rkt" ast))
         (all-from-out (submod "expander.rkt" primitive-ast)))

(module unsafe typed/racket/no-check
  (require typed/racket/unsafe)
  (require (submod "types.rkt" ast))
  (require "globals.rkt")
  (require (submod "expander.rkt" expanders))
  (require (submod "expander.rkt" shrinkers))

  (unsafe-provide (all-defined-out))

  ; Typed Racket has trouble applying arbitrary procedures to an argument list.
  ; So we put those operations in this module.

  (: lookup-macro (-> Symbol pyr-macro))
  (define (lookup-macro name)
    (namespace-variable-value name #t #f (*available-macros*)))

  (: expand-macro (-> pyr-macro-application Pyramid))
  (define (expand-macro exp)
    (let* ([name (pyr-macro-application-name exp)]
           [macro (lookup-macro name)]
           [macro-func (pyr-macro-func macro)]
           [macro-opts (pyr-macro-out-options macro)]
           [result (parameterize ([ current-namespace (*macro-namespace*) ])
                     (let ([ stx (pyr-macro-application-app-syntax exp)])
                       (macro-func stx)))]
           )
      (expand-pyramid result #:options macro-opts))) ; Reparse it to allow macros to return quoted forms.

  (: install-macro! (-> Symbol pyr-macro Void))
  (define (install-macro! name mac)
    (namespace-set-variable-value! name mac #t (*available-macros*)))

  (: install-macro-exp! (-> pyr-macro-definition Void))
  (define (install-macro-exp! exp)
    (let* ([ name (pyr-macro-definition-name exp)]
           [ args (pyr-macro-definition-parameters exp)]
           [ body (pyr-macro-definition-body exp)]
           [ macro-exp #`(lambda (#,@args) #,body)]
           [ macro-func (eval macro-exp (*macro-namespace*))]
           [ macro-out-opts (pyr-macro-definition-out-options exp)]
           [ macro (pyr-macro macro-func macro-out-opts)]
           )
      (install-macro! name macro)
      ))
)

(unsafe-require/typed 'unsafe
  [ lookup-macro (-> Symbol pyr-macro)]
  [ expand-macro (-> pyr-macro-application Pyramid)]
  [ install-macro! (-> Symbol pyr-macro Void)]
  [ install-macro-exp! (-> pyr-macro-definition Void)]
  )

(: install-macro-function! (-> Symbol PyrMacroFunction Void))
(define (install-macro-function! name func)
  (install-macro! name (pyr-macro func (*expander-options*))))

(: macro? (-> Symbol Boolean))
(define (macro? x) (namespace-contains? (*available-macros*) x))

(define (variable? exp) (symbol? exp))

(: transform-ast-children (-> Pyramid (-> Pyramid Pyramid) Pyramid))
(define (transform-ast-children x f)
  (match x
    [(struct pyr-const _)                x]
    [(struct pyr-variable _)             x]
    [(struct pyr-quoted _)               x]
    [(struct pyr-assign (name value))    (pyr-assign name (f value))]
    [(struct pyr-definition (name body)) (pyr-definition name (f body))]
    [(struct pyr-lambda (vars body))     (pyr-lambda vars (f body))]
    [(struct pyr-if (pred cons alt))     (pyr-if (f pred) (f cons) (f alt))]
    [(struct pyr-begin (actions))        (sequence->exp (map f actions))]
    [(struct pyr-macro-definition _)     x]
    [(struct pyr-macro-application _)    x] ; NOTE: Macros are simplification boundaries until expanded.
    [(struct pyr-asm _)                  x]
    [(struct pyr-application (op xs dot)) (pyr-application (f op) (map f xs) (if dot (f dot) #f))]
    [(struct pyr-unknown-application _) x]
    ))

(: ast-map-on (All (A) (-> (-> Pyramid Boolean : #:+ A) (-> A Pyramid) (-> Pyramid Pyramid))))
(define (ast-map-on pred f)
  (λ (x) (if (pred x) (f x) x)))

(: transform-ast-descendants (-> Pyramid (-> Pyramid Pyramid) Pyramid))
(define (transform-ast-descendants x f)
  (f (transform-ast-children x (λ (x) (transform-ast-descendants x f)))))

(: transform-ast-children-on (All (A) (-> Pyramid (-> Pyramid Boolean : #:+ A) (-> A Pyramid) Pyramid)))
(define (transform-ast-children-on prog pred f)
  (transform-ast-children prog (ast-map-on pred f)))

(: transform-ast-descendants-on (All (A) (-> Pyramid (-> Pyramid Boolean : #:+ A) (-> A Pyramid) Pyramid)))
(define (transform-ast-descendants-on prog pred f)
  (transform-ast-descendants prog (ast-map-on pred f)))

(: children (-> Pyramid Pyramids))
(define (children prog)
  (let ([ ret : Pyramids null ])
    (: add-child (-> Pyramid Pyramid))
    (define (add-child child)
      (set! ret (cons child ret))
      child)
    (transform-ast-children prog add-child)
    ret))

(: descendants (-> Pyramid Pyramids))
(define (descendants prog)
  (let ([ ret : Pyramids null ])
    (: add-child (-> Pyramid Pyramid))
    (define (add-child child)
      (set! ret (cons child ret))
      child)
    (transform-ast-descendants prog add-child)
    ret))

(: all-syntax (All (A) (-> (-> Pyramid Boolean : #:+ A) Pyramid (Listof A))))
(define (all-syntax pred prog) (filter pred (descendants prog)))

(: all-definitions (-> Pyramid pyr-definitions))
(define (all-definitions prog) (all-syntax pyr-definition? prog))

(: all-variables (-> Pyramid pyr-variables))
(define (all-variables prog) (all-syntax pyr-variable? prog))

(: all-macro-definitions (-> Pyramid pyr-macro-definitions))
(define (all-macro-definitions prog) (all-syntax pyr-macro-definition? prog))

(: all-macro-applications (-> Pyramid pyr-macro-applications))
(define (all-macro-applications prog) (all-syntax pyr-macro-application? prog))

(: all-applications (-> Pyramid pyr-applications))
(define (all-applications prog) (all-syntax pyr-application? prog))

(: all-lambdas (-> Pyramid pyr-lambdas))
(define (all-lambdas prog) (all-syntax pyr-lambda? prog))

(: all-assigns (-> Pyramid pyr-assigns))
(define (all-assigns prog) (all-syntax pyr-assign? prog))

(define (new-label-number) (tick-counter! *label-counter*))

(: make-label-name (-> Symbol Symbol))
(define (make-label-name name)
  (string->symbol
   (string-append
    (symbol->string name)
    "-"
    (number->string (*abstract-offset*))
    "-"
    (number->string (new-label-number)))))

(: make-label (case-> (-> Symbol label-definition)
                      (-> Symbol Integer label-definition)
                      (-> Symbol Integer Boolean label-definition)))
(define (make-label name [offset 0] [virtual? #f])
  (label-definition (make-label-name name)
                    offset
                    virtual?
                    ))
(: make-macro-application (-> (Syntaxof Any) pyr-macro-application))
(define (make-macro-application stx)
  ; (some-macro . anything)
  (: name Symbol)
  (define name (car (cast (syntax->datum stx) (Pairof Symbol Any))))
  (pyr-macro-application name (unsafe-cast stx))
  )
