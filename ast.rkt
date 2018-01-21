#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "globals.rkt")

(provide (all-defined-out))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(: pyramid? (-> Any Boolean))
(define (pyramid? x)
  (or
   (self-evaluating? x)
   (quoted? x)
   (variable? x)
   (assignment? x)
   (definition? x)
   (lambda? x)
   (if? x)
   (begin? x)
   (application? x)
   (macro? x)
   (macro-application? x)
   (asm? x)
   ))


(: exp-type (-> Pyramid Void))
(define (exp-type exp)
  (cond ((self-evaluating? exp)   'self-evaluating)
        ((quoted? exp)            'quoted)
        ((macro? exp)             'macro)
        ((variable? exp)          'variable)
        ((assignment? exp)        'assignment)
        ((definition? exp)        'definition)
        ((if? exp)                'if)
        ((lambda? exp)            'lambda)
        ((begin? exp)             'begin)
        ((macro-application? exp) 'macro-application)
        ((application? exp)       'application)
        (else
         (error "Unknown expression type -- exp-type" exp))))

(: assert-pyramid (-> Any Any))
(define (assert-pyramid x)
  (if (pyramid? x)
      x
      (error "assert-pyramid: Not a valid value" x)))

(: quoted? (-> Pyramid Boolean))
(define (quoted? exp)
  (tagged-list? exp 'quote))

(: text-of-quotation (-> PyrQuote Pyramid))
(define (text-of-quotation exp) (cadr exp))

(: tagged-list? (-> Pyramid Symbol Boolean))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (variable? exp) (symbol? exp))

(: assignment? (Pyramid -> Boolean))
(define (assignment? exp)
  (tagged-list? exp 'set!))

(: assignment-variable (-> PyrAssign VariableName))
(define (assignment-variable (exp : PyrAssign)) (cadr exp))

(: assignment-value (-> PyrAssign Pyramid))
(define (assignment-value (exp : PyrAssign)) (caddr exp))

(: make-assignment (-> VariableName Pyramid PyrAssign))
(define (make-assignment var val) (list 'set! var val))

(: definition? (-> Pyramid Boolean))
(define (definition? exp)
  (tagged-list? exp 'define))

(: definition-variable (-> PyrDefinition VariableName))
(define (definition-variable (exp : PyrDefinition))
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

; '(define x 5)
; cdr = (x 5)
; cddr = (5)
; caddr = 5
; OR
; '(define (f x y) (+ x y))
; cdr = ((f x y) (+ x y))
; cadr = (f x y)
; cdadr = (x y)
; cddr = (+ x y)
(: definition-value (-> PyrDefinition Pyramid))
(define (definition-value (exp : PyrDefinition))
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(: make-definition (-> VariableName Pyramid PyrDefinition))
(define (make-definition var val) (list 'define var val))

; '(lambda (x y) (+ x y))
(: lambda? (-> Pyramid Boolean))
(define (lambda? exp) (or (tagged-list? exp 'lambda)
                          (tagged-list? exp 'λ)))

(: lambda-parameters (-> PyrLambda VariableNames))
(define (lambda-parameters exp)
  (if (= 2 (length exp))
      null
      (if (symbol? (cadr exp))
          (list (cadr exp))
          (cadr exp))))
      

(: lambda-body (-> PyrLambda Sequence))
(define (lambda-body (exp : PyrLambda))
  (if (= 2 (length exp))
      (cdr exp)
      (cddr exp)))

(: make-lambda (-> VariableNames Pyramid PyrLambda))
(define (make-lambda parameters body)
  (list* 'lambda parameters (if (null? body) '((begin)) body)))

; '(if pred cons alt)
; OR
; '(if pred cons)
(: if? (-> Pyramid Boolean))
(define (if? exp) (tagged-list? exp 'if))

(: if-predicate (-> PyrIf Pyramid))
(define (if-predicate exp) (cadr exp))

(: if-consequent (-> PyrIf Pyramid))
(define (if-consequent exp) (caddr exp))

(: if-alternative (-> PyrIf Pyramid))
(define (if-alternative (exp : PyrIf))
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(: make-if (-> Pyramid Pyramid Pyramid Pyramid))
(define (make-if pred cons alt) (list 'if pred cons alt))

; '(begin <actions>)
(: begin? (-> Pyramid Boolean))
(define (begin? exp) (tagged-list? exp 'begin))
(: begin-actions (-> PyrBegin Sequence))
(define (begin-actions exp) (cdr exp))

(: last-exp? (-> Sequence Boolean))
(define (last-exp? seq) (null? (cdr seq)))
(: first-exp (-> Sequence Pyramid))
(define (first-exp seq) (car seq))
(: rest-exps (-> Sequence Sequence))
(define (rest-exps seq) (cdr seq))

(: make-begin (-> (Listof Pyramid) PyrBegin))
(define (make-begin xs) (cons 'begin xs))

(define syntaxes '(quote set! define if lambda begin defmacro push op byte label asm))

; '(f x y)
(: application? (-> Pyramid Boolean))
(define (application? exp)
  (and (pair? exp)
       (not (member (car exp) syntaxes))
       (implies (symbol? (operator exp)) (not (namespace-contains? (*available-macros*) (operator exp))))
       ))

(: operator (-> PyrApplication Pyramid))
(define (operator exp) (car exp))
(: operands (-> PyrApplication Sequence))
(define (operands exp) (cdr exp))

(: no-operands? (-> (Listof Pyramid) Boolean))
(define (no-operands? ops) (null? ops))
(: first-operand (-> (Listof Pyramid) Pyramid))
(define (first-operand ops) (car ops))
(: rest-operands (-> (Listof Pyramid) (Listof Pyramid)))
(define (rest-operands ops) (cdr ops))

(: make-application (-> Pyramid Pyramids PyrApplication))
(define (make-application operator operands) (cons operator operands))

(: sequence->exp (-> Sequence Pyramid))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

#|
Macro example:
(defmacro (duplicate x)
  (if (definition? x)
      x
      `(begin
         ,x
         ,x)))
|#

(: macro? (-> Pyramid Boolean))
(define (macro? exp) (tagged-list? exp 'defmacro))

(define macro-variable definition-variable)
(define (macro-args exp) (cdadr exp))
(define (macro-body exp) (make-begin (cddr exp)))

(: expand-macro (-> PyrApplication Pyramid))
(define (expand-macro exp)
  (let* ((name (operator exp))
         (macro (namespace-variable-value name #t #f (*available-macros*)))
         (result (parameterize ([ current-namespace (*macro-namespace*) ])
                   (apply macro (operands exp))))
         )
    result))

(: macro-application? (-> Pyramid Boolean))
(define (macro-application? exp)
  (and (pair? exp)
       (symbol? (operator exp))
       (namespace-contains? (*available-macros*) (operator exp))
       ))

(: install-macro! (-> Symbol Procedure Void))
(define (install-macro! name func)
  (namespace-set-variable-value! name func #t (*available-macros*))
  )

(: install-macro-exp! (-> Pyramid Void))
(define (install-macro-exp! exp)
  (let* ((mac-name (macro-variable exp))
         (arg-names (macro-args exp))
         (mac-body (macro-body exp))
         (macro-exp `(lambda ,arg-names ,mac-body))
         (macro (eval macro-exp (*macro-namespace*)))
         )
    (install-macro! mac-name macro)
    ))

(define (namespace-contains? namespace name)
  (namespace-variable-value name #f (λ () #f) namespace)
  )

(: asm? (-> Pyramid Boolean))
(define (asm? exp) (tagged-list? exp 'asm))

(define (asm-insts exp) (cdr exp))

(define (make-asm exps) (cons 'asm exps))

(: transform-ast-children (-> Pyramid (-> Pyramid Pyramid) Pyramid))
(define (transform-ast-children x f)
  (cond [(quoted?   x) x]
        [(variable? x) x]
        [(assignment? x) (make-assignment (assignment-variable x)
                                          (f (assignment-value x)))]
        [(definition? x) (make-definition (definition-variable x)
                                          (f (definition-value x)))]
        [(lambda? x) (make-lambda (lambda-parameters x)
                                  (map f (lambda-body x)))]
        [(if? x) (make-if (f (if-predicate x))
                          (f (if-consequent x))
                          (f (if-alternative x)))]
        [(begin? x) (make-begin (map f (begin-actions x)))]
        [(macro? x) x]
        [(macro-application? x) x ] ; TODO: Should we recurse into a macro's args?
        [(asm? x) x]
        [(application? x) (make-application (f (operator x))
                                            (map f (operands x)))]

        [else x]
        ))

(: ast-map-on (-> (-> Pyramid Boolean) (-> Pyramid Pyramid) (-> Pyramid Pyramid)))
(define (ast-map-on pred f)
  (λ (x) (if (pred x) (f x) x)))

(: transform-ast-descendants (-> Pyramid (-> Pyramid Pyramid) Pyramid))
(define (transform-ast-descendants x f)
  (f (transform-ast-children x (λ (x) (transform-ast-descendants x f)))))


(: transform-ast-children-on (-> Pyramid (-> Pyramid Boolean) (-> Pyramid Pyramid) Pyramid))
(define (transform-ast-children-on prog pred f)
  (transform-ast-children prog (ast-map-on pred f)))


(: transform-ast-descendants-on (-> Pyramid (-> Pyramid Boolean) (-> Pyramid Pyramid) Pyramid))
(define (transform-ast-descendants-on prog pred f)
  (transform-ast-descendants prog (ast-map-on pred f)))


(: children (-> Pyramid (Listof Pyramid)))
(define (children prog)
  (let ([ ret null ])
    (define (add-child child)
      (set! ret (cons child ret))
      child)
    (transform-ast-children prog add-child)
    ret))

(define (descendants prog)
  (let ([ ret null ])
    (define (add-child child)
      (set! ret (cons child ret))
      child)
    (transform-ast-descendants prog add-child)
    ret))

(define (all-syntax pred prog) (filter pred (descendants prog)))

(define (all-definitions prog) (all-syntax definition? prog))
(define (all-variables prog) (all-syntax variable? prog))
(define (all-macros prog) (all-syntax macro? prog))
(define (all-macro-applications prog) (all-syntax macro-application? prog))
(define (all-applications prog) (all-syntax application? prog))
(define (all-lambdas prog) (all-syntax lambda? prog))

