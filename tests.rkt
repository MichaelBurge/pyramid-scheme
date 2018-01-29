#lang racket

;(require "minimize.rkt")
;; (require "test.rkt")
(require "macro.rkt")
;; (require "globals.rkt")
;; (require "ast.rkt")
(require "types.rkt")
 (require "parser.rkt")
(require "simplifier.rkt")
(require "io.rkt")

;(sequence->exp (list (pyr-variable 'a)))

(%-install-macro-library!)

; (print-ast (pass-expand-macros (expand-pyramid '((5)))))

(print-ast
(pass-expand-macros
(pass-expand-macros
 (pass-expand-macros
   (expand-pyramid
    '(begin
       (include psl "syntax.pmd")
       (define (f a b) a)
       (let* ([ x 5 ]       ; 5
              [ y (f x 7) ] ; 12
              [ z (f x y) ] ; 17
              )
         (f y z)) ; 29
       ))
  )
 )
)
)

;; (expand-pyramid
;;  `(begin
;;     (defmacro (m a b . cs) 5)))

;; (expand-pyramid
;;  '(defmacro (func-return [ save? #t ])
;;     `(asm
;;       ,@(maybe->list save? '(cg-write-reg (reg 'val) stack))
;;       (cg-goto (reg 'continue)))))

;; (expand-pyramid
;;  '(define (update-sender-balance! a b)
;;     (let* ((old-balance (%-store-read **sender**))
;;            (new-balance (+ old-balance **sent-value**)))
;;       (%-store-write! **sender** new-balance)
;;       new-balance)))
;; (make-test-suite
;;  (list
;;  `(case "Deposits"
;;     (init (sender (quote alice)) (value 0))
;;     (txn (sender (quote bob)) (value 100)(assert-balance (quote alice) 100) (assert-return 100))
;;     (txn (sender (quote bob)) (value 150) (assert-balance (quote alice) 250) (assert-return 250))
;;     (txn (sender (quote charlie)) (value 0) (data (sender (quote charlie))) (assert-return 0))
;;     (txn (sender (quote charlie)) (value 0) (data (sender (quote bob))) (assert-return 250))
;;     (txn (sender (quote charlie)) (value 100) (data (sender (quote bob))) (assert-return 250))
;;     (txn (sender (quote charlie)) (value 100) (data (sender (quote charlie))) (assert-return 200))
;;     (txn (sender (quote charlie)) (value 0) (assert-balance (quote alice) 450) (assert-return 200)))))
;; (assert-equal "Reductions const" '() (reductions 5))
;; (assert-equal "Reductions list" '(()) (reductions-list '(5)))
;; (assert-equal "Reductions apply" '(0) (reductions '(x)))
;; (assert-equal "Reductions 1" '((begin)) (reductions '(begin 5)))
;; (assert-equal "Reductions begin nested"
;;               '((begin (begin 2 (begin 3)))
;;                 (begin 1) (begin 1 (begin (begin 3)))
;;                 (begin 1 (begin 2))
;;                 (begin 1 (begin 2 (begin)))
;;                 (begin 1 (begin 2 3))
;;                 (begin 1 2)
;;                 (begin 1 (begin 3)))
;;               (reductions '(begin 1 (begin 2 (begin 3)))))
;; (assert-equal "Reductions define" '((define x 0) (define x 5)) (reductions '(define x (f 5))))
;; (assert-equal "Reductions define 2" '((begin) (begin (define x 0)) (begin (define x 5))) (reductions '(begin (define x (f 5)))))
;; ;(assert-equal "Reductions lambda" '(0) '(lambda () (begin)))
;; (assert-equal "Minimize begin 0" '(begin) (minimize pred? '(begin)))
;; (assert-equal "Minimize begin 1" '(begin) (minimize pred? '(begin 5)))
;; (assert-equal "Minimize begin nested 0" '(begin) (minimize pred? '(begin (begin (begin)))))
;; (assert-equal "Minimize begin nested 1" '(begin) (minimize pred? '(begin 1 (begin 2 (begin)))))
