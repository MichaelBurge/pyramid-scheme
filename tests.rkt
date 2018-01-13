#lang racket

(require "minimize.rkt")
(require "test.rkt")

(define (pred? x) #t)

(assert-equal "Reductions const" '() (reductions 5))
(assert-equal "Reductions list" '(()) (reductions-list '(5)))
(assert-equal "Reductions apply" '(0) (reductions '(x)))
(assert-equal "Reductions 1" '((begin)) (reductions '(begin 5)))
(assert-equal "Minimize begin 0" '(begin) (minimize pred? '(begin)))
(assert-equal "Minimize begin 1" '(begin) (minimize pred? '(begin 5)))
