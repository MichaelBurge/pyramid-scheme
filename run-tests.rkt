#lang typed/racket/no-check

; (require test-engine/racket-tests)
(require racket/trace)

(require "types.rkt")
(require "ast.rkt")
(require "interpreter.rkt")
(require "compiler.rkt")
(require "evaluator.rkt")
(require "io.rkt")
(require "codegen.rkt")
(require "serializer.rkt")

; TODO: Turn these into unit tests.
; TEST 1: (cg-intros (list (const 1234) (const 4321)))
; TEST 2:
(cg-allocate-initialize (const 3) (list (const 10) (const 20) (const 30)))

(: full-debug-output (-> Pyramid Void))
(define (full-debug-output prog)
  (let* ((the-global-environment (setup-environment))
         (instructions           (compile-pyramid prog 'val 'next))
         (eth-instructions       (codegen (inst-seq-statements instructions))))
    (begin
      (newline) (display "Abstract Instructions:") (newline)
      (display-all (inst-seq-statements instructions))
  
      (newline) (display "EVM Instructions:") (newline)

      (display-all eth-instructions)
      (serialize-print eth-instructions))))

(: prog-const Pyramid)
(define prog-const 1234)

(full-debug-output prog-const)

; (serialize-print (codegen-one (assign 'val (const 9999))))

;; (: prog-assign Pyramid)
;; (define prog-assign
;;   '(define x 1234))

;; (: prog-factorial Pyramid)
;; (define prog-factorial 
;;   '(define (factorial n)
;;      (if (= n 1)
;;          1
;;          (* (factorial (- n 1)) n))))

;; (define prog prog-const)






;; ; (struct->vector (op 'derp (const 5)))

;; (display-all eth-instructions)

;; (serialize-print eth-instructions)
