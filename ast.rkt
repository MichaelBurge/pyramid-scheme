#lang typed/racket/no-check

(require "types.rkt")
(require "globals.rkt")

(provide (all-defined-out))
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(: pyramid? (-> Any Boolean))
(define (pyramid? x)
  (or (quoted? x)
      (variable? x)
      (assignment? x)
      (definition? x)
      (lambda? x)
      (if? x)
      (begin? x)
      (application? x)
      (cond? x)
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
        ((cond? exp)              'cond)
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

(: make-lambda (-> (Listof Pyramid) Pyramid PyrLambda))
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

; '(f x y)
(: application? (-> Pyramid Boolean))
(define (application? exp) (pair? exp))
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

;;;**following needed only to implement COND as derived expression,
;;; not needed by eceval machine in text.  But used by compiler

;; from 4.1.2
(: make-if (-> Pyramid Pyramid Pyramid Pyramid))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(: sequence->exp (-> Sequence Pyramid))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(: make-begin (-> Sequence Pyramid))
(define (make-begin seq) (cons 'begin seq))

; '(cond (pred1 (act11 act12)) (pred2 (act21 act22 act23)) ... [(else (actn1 actn2))])
(: cond? (-> Pyramid Boolean))
(define (cond? exp) (tagged-list? exp 'cond))
(: cond-clauses (-> PyrCond (Listof PyrCondClause)))
(define (cond-clauses exp) (cdr exp))
(: cond-else-clause? (-> PyrCondClause Boolean))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(: cond-predicate (-> PyrCondClause Pyramid))
(define (cond-predicate clause) (car clause))
(: cond-actions (-> PyrCondClause Sequence))
(define (cond-actions clause) (cdr clause))

(: cond->if (-> PyrCond (U 'false Pyramid)))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(: expand-clauses (-> (Listof PyrCondClause) (U 'false Pyramid)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
;; end of Cond support

#|
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
(define (macro-body exp) (caddr exp))

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
  (if (application? exp)
      (let ((name (operator exp)))
        (if (symbol? name)
            (namespace-contains? (*available-macros*) name)
            #f))
      #f))

(define (namespace-contains? namespace name)
  (namespace-variable-value name #f (λ () #f) namespace)
  )

(: asm? (-> Pyramid Boolean))
(define (asm? exp) (tagged-list? exp 'asm))

(define (asm-insts exp) (cdr exp))
