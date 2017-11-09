#lang typed/racket

(define-type VariableName Symbol)
(define-type VariableNames (Listof VariableName))

(define-type PyrSelfEvaluating (U Number String))
(define-type PyrVariable Symbol)
(define-type PyrQuote (Pairof Symbol (Pairof Pyramid Any)))
(define-type PyrAssign (Pairof Symbol (Pairof PyrVariable Pyramid)))
(define-type PyrDefinition (Pairof Symbol (Pairof (Pairof VariableName (Listof VariableName)) Pyramid)))
(define-type PyrIf (Listof (U Symbol Pyramid)))
(define-type PyrLambda (Pairof Symbol (Pairof VariableNames Sequence)))
(define-type PyrBegin (Pairof Symbol (Listof Pyramid)))
(define-type PyrCondClause (U PyrCondPred PyrCondElse))
(define-type PyrCondPred (Pairof Pyramid Sequence))
(define-type PyrCondElse (Pairof Symbol Sequence))
(define-type PyrCond (Pairof Symbol (Listof PyrCondClause)))
(define-type PyrApplication (Pairof Pyramid Sequence))
(define-type Pyramid (U PyrSelfEvaluating
                        PyrVariable
                        PyrQuote
                        PyrAssign
                        PyrDefinition
                        PyrIf
                        PyrLambda
                        PyrBegin
                        PyrCond
                        PyrApplication))

(define-type Value Any)
(define-type Sequence (Listof Pyramid))
(define-type PrimOps Any)
(define-type Assertion Any)

(provide (all-defined-out))
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


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
(define (assignment-value (exp : PyrAssign)) (cddr exp))

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
(define (lambda? exp) (tagged-list? exp 'lambda))

(: lambda-parameters (-> PyrLambda VariableNames))
(define (lambda-parameters exp) (cadr exp))

(: lambda-body (-> PyrLambda Sequence))
(define (lambda-body (exp : PyrLambda)) (cddr exp))

(: make-lambda (-> (Listof Pyramid) Pyramid Pyramid))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

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
