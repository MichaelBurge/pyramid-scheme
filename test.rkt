#lang typed/racket/no-check

; (require test-engine/racket-tests)
(require racket/cmdline)
(require json)
(require binaryio/integer)

(require "types.rkt")
(require "ast.rkt")
(require "compiler.rkt")
(require "evaluator.rkt")
(require "io.rkt")
(require "codegen.rkt")
(require "serializer.rkt")
(require "disassembler.rkt")
(require "utils.rkt")
(require "simulator.rkt")
(require "analysis.rkt")
(require "macro.rkt")
(require "globals.rkt")

(provide (all-defined-out))

(define MAX-ITERATIONS 1000000)

(define (assert-equal name expected actual-bs)
  (let ([ actual (parse-pyramid-result expected actual-bs) ])
    (if (equal? expected actual)
        (begin
          (display `("Test Passed: " ,name ,expected ,actual))
          (newline))
        (error "Test failed: " name expected actual))
    ))

(define (parse-pyramid-result reference bs)
  (cond ((fixnum? reference) (bytes->integer bs #f #t))
        (else (error "Unsupported value - parse-pyramid-result:" reference))))

(define (on-simulate-nop vm i reads) (void))

(define (on-simulate-debug reverse-symbol-table vm i reads)
  (fprintf (current-output-port) "~a" (evm-step vm))
  (write-char #\tab)
  (display (label-name (dict-ref reverse-symbol-table (evm-pc vm) (label ""))))
  (write-char #\tab)
  (display (integer->hex (evm-pc vm)))
  (write-char #\tab)
  (write-json (evm-stack vm))
  (write-char #\tab)
  (write-json (memory-dict vm))
  (write-char #\tab)
  (display i)
  (newline)
  )

(define (on-error-throw vm)
  (error "Test Failure - exception thrown")
  )

(: run-until-return (-> Bytes Bytes))
(define (run-until-return bs)
  (let* ([ result null ]
         [ reverse-symbol-table (invert-dict *symbol-table*) ]
         [ on-simulate (if (*verbose?*)
                           (λ (vm i reads) (on-simulate-debug reverse-symbol-table vm i reads))
                           on-simulate-nop)
                       ]
         [ vm (make-vm bs
                       on-simulate
                       (λ (vm bs) (set! result bs))
                       on-error-throw) ])
    (simulate! vm MAX-ITERATIONS)
    result))
    

; A test is a regular Pyramid program that uses special test macros to communicate with the compiler.
(: run-test (-> String Pyramid Void))
(define (run-test name prog)
  (let* ([ params (full-compile prog) ]
         [ initializer-bs (third params) ]
         [ program-bs (run-until-return initializer-bs) ]
         [ actual-result (run-until-return program-bs) ]
         )
    (assert-equal name **test-expected-result** actual-result)))

; TODO: Turn these into unit tests.
; TEST 1: (cg-intros (list (const 1234) (const 4321)))
; TEST 2: (cg-allocate-initialize (const 3) (list (const 10) (const 20) (const 30)))
; TEST 3: (codegen (inst-seq-statements (compile-pyramid 5 'val 'next))) ; integer should be boxed
; TEST 4: (codegen (list (assign 'val (op 'box (list (const 5))))))
; TEST 5: (codegen-one (save 'continue))
; TEST 6: (display-all (cg-intros (list (const 1) (const 2) (const 3) (const 4))))
; TEST 7: (eq? #f (stack-read? (op 'lookup-variable-value `(,(const 'factorial) ,(reg 'env)))))
; TEST 8: (push-true-size (eth-push 'shrink 256))

; (serialize-print (codegen-one (assign 'val (const 9999))))
; (display-all (inst-seq-statements (compile-pyramid prog-application 'val 'next)))
; (full-debug-output prog-macro)

