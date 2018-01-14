#lang racket

(require "minimize.rkt")
(require "test.rkt")
(require "macro.rkt")
(require "globals.rkt")

(define (pred? x) #t)

(assert-equal "Reductions const" '() (reductions 5))
(assert-equal "Reductions list" '(()) (reductions-list '(5)))
(assert-equal "Reductions apply" '(0) (reductions '(x)))
(assert-equal "Reductions 1" '((begin)) (reductions '(begin 5)))
(assert-equal "Reductions begin nested"
              '((begin (begin 2 (begin 3)))
                (begin 1) (begin 1 (begin (begin 3)))
                (begin 1 (begin 2))
                (begin 1 (begin 2 (begin)))
                (begin 1 (begin 2 3))
                (begin 1 2)
                (begin 1 (begin 3)))
              (reductions '(begin 1 (begin 2 (begin 3)))))
(assert-equal "Reductions define" '((define x 0) (define x 5)) (reductions '(define x (f 5))))
(assert-equal "Minimize begin 0" '(begin) (minimize pred? '(begin)))
(assert-equal "Minimize begin 1" '(begin) (minimize pred? '(begin 5)))
(assert-equal "Minimize begin nested 0" '(begin) (minimize pred? '(begin (begin (begin)))))
(assert-equal "Minimize begin nested 1" '(begin) (minimize pred? '(begin 1 (begin 2 (begin)))))

;; (*include-directory* "tests")
;; (%-install-macro-library)
;; (assert-equal "Include" (list (expand-macro (reductions '(include psl "arith.pmd")))))