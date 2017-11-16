#lang typed/racket/no-check

(require test-engine/racket-tests)
(require racket/trace)

(require "types.rkt")
(require "ast.rkt")
(require "interpreter.rkt")
(require "compiler.rkt")
(require "evaluator.rkt")
(require "io.rkt")

(: eval-semicompiled-pyramid (-> Pyramid Value))
(define (eval-semicompiled-pyramid prog)
  (let* ((text (inst-seq-statements (compile-pyramid prog 'val 'next)))
         )
         ;; (machine (make-pyramid-machine text)))
    text))
    ;; (machine 'start)
    ;; (machine 'get-register 'val)))

(: prog-factorial Pyramid)
(define prog-factorial 
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

(define the-global-environment (setup-environment))
(display-all (inst-seq-statements (compile-pyramid prog-factorial 'val 'next)))
; (eval-semicompiled-pyramid prog-factorial)
;; (eval-pyramid prog-factorial the-global-environment)
;; (eval-pyramid '(factorial 5) the-global-environment)
; (check-expect 
