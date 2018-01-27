#lang errortrace typed/racket

(require typed/racket/unsafe)
(require "types.rkt")
(require "globals.rkt")
(require "parser.rkt")
(unsafe-require/typed "unsafe.rkt"
                      [ unsafe-eval (-> Any Namespace (-> Pyramid * Pyramid))]
                      [ unsafe-apply (-> Procedure (Listof Any) Any)]
                      [ unsafe-cast (All (A B) (-> A B))]
                      )

(provide (all-defined-out))

(define (variable? exp) (symbol? exp))

(: sequence->exp (-> Pyramids Pyramid))
(define (sequence->exp seq)
  (match seq
    (`() (pyr-begin '()))
    (`(list ,x) x)
    (xs (pyr-begin xs))))

(: expand-macro (-> pyr-macro-application Pyramid))
(define (expand-macro exp)
  (let* ((name (pyr-macro-application-name exp))
         (macro (lookup-macro name))
         (result (parameterize ([ current-namespace (*macro-namespace*) ])
                   (let ([ args (map shrink-pyramid (pyr-macro-application-operands exp))])
                     (displayln `(,name ,@args ,(procedure-arity macro) ,macro))
                     (unsafe-apply macro args))))
         )
    (expand-pyramid result))) ; Reparse it to allow macros to return quoted forms.

(: lookup-macro (-> Symbol PyrMacro))
(define (lookup-macro name)
  (unsafe-cast (namespace-variable-value name #t #f (*available-macros*))))

(: install-macro! (-> Symbol Procedure Void))
(define (install-macro! name func)
  (namespace-set-variable-value! name func #t (*available-macros*)))

(: install-macro-exp! (-> pyr-macro-definition Void))
(define (install-macro-exp! exp)
  (let* ([ name (pyr-macro-definition-name exp)]
         [ args (pyr-macro-definition-parameters exp)]
         [ body (pyr-macro-definition-body exp)]
         [ macro-exp `(lambda ,args ,body)]
         [ macro (unsafe-eval macro-exp (*macro-namespace*))]
         )
    (install-macro! name macro)
    ))

(: transform-ast-children (-> Pyramid (-> Pyramid Pyramid) Pyramid))
(define (transform-ast-children x f)
  (match x
    [(struct pyr-const (v))              x]
    [(struct pyr-variable (v))           x]
    [(struct pyr-quoted (exp))           x]
    [(struct pyr-assign (name value))    (pyr-assign name (f value))]
    [(struct pyr-definition (name body)) (pyr-definition name (f body))]
    [(struct pyr-lambda (vars body))     (pyr-lambda vars (f body))]
    [(struct pyr-if (pred cons alt))     (pyr-if (f pred) (f cons) (f alt))]
    [(struct pyr-begin (actions))        (pyr-begin (map f actions))]
    [(struct pyr-macro-definition _)     x]
    [(struct pyr-macro-application _)    x] ; TODO: Should we recurse into a macro's args?
    [(struct pyr-asm _)                  x]
    [(struct pyr-application (op xs))    (pyr-application (f op) (map f xs))]
    [_ (error "transform-ast-children: Unknown syntax" x)]
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

