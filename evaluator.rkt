#lang typed/racket

(require typed/racket/unsafe)
(require "ast.rkt")
(require rnrs/mutable-pairs-6)
(unsafe-require/typed "unsafe.rkt" [ unsafe-apply (-> Procedure Any * Any) ])

(provide (all-defined-out))

(define-type Frame (Pairof (MListof VariableName) (MListof Value)))
(define-type Environment (MListof Frame))

(define-type Values (Listof Value))

(struct primitive ([ implementation : Procedure ]))
(struct procedure ([ parameters : VariableNames ] [ body : Sequence ] [ environment : Environment ]))

(: pyramid-eval (-> Pyramid Environment Value))
(define (pyramid-eval exp env)
  (cond ((self-evaluating? exp) (cast exp PyrSelfEvaluating))
        ((variable? exp) (lookup-variable-value (cast exp PyrVariable) env))
        ((quoted? exp) (text-of-quotation (cast exp PyrQuote)))
        ((assignment? exp) (eval-assignment (cast exp PyrAssign) env))
        ((definition? exp) (eval-definition (cast exp PyrDefinition) env))
        ((if? exp) (eval-if (cast exp PyrIf) env))
        ((lambda? exp)
         (let ((lexp (cast exp PyrLambda)))
           (procedure (lambda-parameters lexp)
                      (lambda-body lexp)
                      env)))
        ((begin? exp) 
         (eval-sequence (begin-actions (cast exp PyrBegin)) env))
        ((cond? exp) (pyramid-eval (cond->if (cast exp PyrCond)) env))
        ((application? exp)
         (let ((aexp (cast exp PyrApplication)))
           (metacircular-apply (cast (pyramid-eval (operator aexp) env) procedure)
                               (list-of-values (operands aexp) env))))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(: metacircular-apply (-> procedure Values Any))
(define (metacircular-apply procedure arguments)
  (cond ((primitive? procedure)
         (apply-primitive-procedure procedure arguments))
        ((procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(: list-of-values (-> Sequence Environment Values))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (pyramid-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(: eval-if (-> PyrIf Environment Value))
(define (eval-if exp env)
  (if (true? (pyramid-eval (if-predicate exp) env))
      (pyramid-eval (if-consequent exp) env)
      (pyramid-eval (if-alternative exp) env)))

(: eval-sequence (-> Sequence Environment Value))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (pyramid-eval (first-exp exps) env))
        (else (pyramid-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(: eval-assignment (-> PyrAssign Environment Void))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (pyramid-eval (assignment-value exp) env)
                       env))

(: eval-definition (-> PyrDefinition Environment Value))
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (pyramid-eval (definition-value exp) env)
    env))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(: enclosing-environment (-> Environment Environment))
(define (enclosing-environment env) (mcdr env))

(: first-frame (-> Environment Frame))
(define (first-frame env) (mcar env))

(: the-empty-environment Environment)
(define the-empty-environment '())

(: make-frame (-> (MListof VariableName) (MListof Value) Frame))
(define (make-frame variables values)
  (cons variables values))

(: frame-variables (-> Frame VariableNames))
(define (frame-variables frame) (mlist->list (car frame)))
(: frame-values (-> Frame Values))
(define (frame-values frame) (mlist->list (cdr frame)))
                       
(: frame-variables! (-> Frame (MListof VariableName)))
(define (frame-variables! frame) (car frame))
(: frame-values! (-> Frame (MListof Value)))
(define (frame-values! frame) (cdr frame))

(: add-binding-to-frame! (-> VariableName Value Frame Void))
(define (add-binding-to-frame! var val frame)
  (push-mlist! (car frame) var)
  (push-mlist! (cdr frame) val))

(: push-mlist! (All (A) (-> (MListof A) A Void)))
(define (push-mlist! xs x)
  (let ((xs2 xs))
    (set-car! xs x)
    (set-cdr! xs xs2)))

(: list->mlist (All (A) (-> (Listof A) (MListof A))))
(define (list->mlist xs)
  (mcons (car xs) (list->mlist (cdr xs))))

(: mlist->list (All (A) (-> (MListof A) (Listof A))))
(define (mlist->list xs)
  (cons (mcar xs) (mlist->list (mcdr xs))))

(: extend-environment (-> VariableNames Values Environment Environment))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (mcons (make-frame (list->mlist vars) (list->mlist vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(: lookup-variable-value (-> VariableName Environment Value))
(define (lookup-variable-value var env)
  (: env-loop (-> Environment Value))
  (define (env-loop env)
    (: scan (-> VariableNames Values Value))
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(: set-variable-value! (-> VariableName Value Environment Void))
(define (set-variable-value! var val env)
  (: env-loop (-> Environment Void))
  (define (env-loop env)
    (: scan (-> (MListof VariableName) (MListof Value) Void))
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables! frame)
                (frame-values! frame)))))
  (env-loop env))

(: define-variable! (-> VariableName Value Environment Void))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (: scan (-> (MListof VariableName) (MListof Value) Void))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))
             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables! frame)
          (frame-values! frame))))

(: setup-environment (-> Environment))
(define (setup-environment)
  (let ((initial-env
         (extend-environment primitive-procedure-names
                             primitive-procedure-objects
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(: primitive-procedures (Listof (Pairof Symbol Procedure)))
(define primitive-procedures
  (list (cons (ann 'car Symbol) (ann car Procedure))
        (cons 'cdr cdr)
        (cons 'cons cons)
        (cons 'null? null?)
        ;;      more primitives
        ))

(: primitive-procedure-names (Listof Symbol))
(define primitive-procedure-names
  (map (ann car (-> (Pairof Symbol Procedure) Symbol)) primitive-procedures))

(: primitive-procedure-objects (Listof primitive))
(define primitive-procedure-objects
  (map primitive (map (ann cdr (-> (Pairof Symbol Procedure) Procedure)) primitive-procedures)))

(: apply-primitive-procedure (-> primitive (Listof Any) Any))
(define (apply-primitive-procedure proc args)
  (unsafe-apply
   (primitive-implementation proc) args))


;; Uncomment below when testing the interpreter
;; (define input-prompt ";;; M-Eval input:")
;; (define output-prompt ";;; M-Eval value:")

;; (define (driver-loop)
;;   (prompt-for-input input-prompt)
;;   (let ((input (read)))
;;     (let ((output (eval input the-global-environment)))
;;       (announce-output output-prompt)
;;       (user-print output)))
;;   (driver-loop))

;; (define (prompt-for-input string)
;;   (newline) (newline) (display string) (newline))

;; (define (announce-output string)
;;   (newline) (display string) (newline))

;; (define (user-print object)
;;   (if (procedure? object)
;;       (display (list 'compound-procedure
;;                      (procedure-parameters object)
;;                      (procedure-body object)
;;                      '<procedure-env>))
;;       (display object)))

;;;Following are commented out so as not to be evaluated when
;;; the file is loaded.
;; (define the-global-environment (setup-environment))
;; (driver-loop)

'METACIRCULAR-EVALUATOR-LOADED
