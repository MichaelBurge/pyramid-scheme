#lang typed/racket

(require test-engine/racket-tests)

(require "interpreter.rkt")
(require "compiler.rkt")

(: eval-pyramid (-> Pyramid Value))
(define (eval-pyramid prog)
  (eval prog))

(: eval-primops (-> PrimOps Machine Machine))
(: machine-result (-> Machine Value))
(: compile-and-eval-pyramid (-> Pyramid Value))
(define (compile-and-eval-pyramid prog)
  (machine-result
   (eval-primops
    (compile-pyramid prog)
    (make-pyramid-machine))))

(: check-program (-> Pyramid Assertion))
(define (check-program prog)
  (let ((expected (eval-pyramid prog))
        (actual (compile-and-eval-pyramid prog)))
    (check-expect actual expected)))

(: prog-factorial Pyramid)
(define (prog-factorial)
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
  'val
  'next)

(check-program prog-factorial)
