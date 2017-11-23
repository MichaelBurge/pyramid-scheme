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
(require "disassembler.rkt")

; TODO: Turn these into unit tests.
; TEST 1: (cg-intros (list (const 1234) (const 4321)))
; TEST 2: (cg-allocate-initialize (const 3) (list (const 10) (const 20) (const 30)))
; TEST 3: (codegen (inst-seq-statements (compile-pyramid 5 'val 'next))) ; integer should be boxed
; TEST 4: (codegen (list (assign 'val (op 'box (list (const 5))))))
; TEST 5: (codegen-one (save 'continue))
; TEST 6: (display-all (cg-intros (list (const 1) (const 2) (const 3) (const 4))))
; TEST 7: (eq? #f (stack-read? (op 'lookup-variable-value `(,(const 'factorial) ,(reg 'env)))))

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
      (let ((bs (serialize-print eth-instructions)))
        (print-disassembly bs)))))

(: prog-const Pyramid)
(define prog-const 1234)

; (serialize-print (codegen-one (assign 'val (const 9999))))

;; (: prog-assign Pyramid)
;; (define prog-assign
;;   '(define x 1234))

(: prog-factorial Pyramid)
(define prog-factorial 
  '((define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n)))
    (factorial 10)))

; (display-all (inst-seq-statements (compile-pyramid prog-factorial 'val 'next)))

(full-debug-output prog-factorial)

;; (define prog prog-const)






;; ; (struct->vector (op 'derp (const 5)))

;; (display-all eth-instructions)

;; (serialize-print eth-instructions)
