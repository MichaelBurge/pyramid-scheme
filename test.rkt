#lang errortrace typed/racket/no-check

; (require test-engine/racket-tests)
(require racket/cmdline)
(require json)
(require binaryio/integer)
(require lazy/force)
(require racket/pretty)

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
(require "minimize.rkt")

(provide (all-defined-out))

(define MAX-ITERATIONS 1000000)

(define (assert-equal name expected actual debug-info)
  (if (equal? expected actual)
      (begin
        (display `("Test Passed: " ,name ,expected ,actual ,debug-info))
        (newline))
      (error "Test failed: " name expected actual))
  )

(define (assert-equal-vm name expected actual-bs debug-info)
  (if (exn:fail? actual-bs)
      (error "assert-equal-vm: Caught error" actual-bs)
      (let ([ actual (parse-type (infer-type expected) actual-bs) ])
        (assert-equal name expected actual debug-info))))

(define (on-simulate-nop vm i reads) (void))

(define (on-simulate-debug reverse-symbol-table vm i reads)
  (fprintf (current-output-port) "~a" (vm-exec-step vm))
  (write-char #\tab)
  (display (label-name (dict-ref reverse-symbol-table (vm-exec-pc vm) (label ""))))
  (write-char #\tab)
  (display (integer->hex (vm-exec-pc vm)))
  (write-char #\tab)
  (write-json (vm-exec-stack vm))
  (write-char #\tab)
  (write-json (memory-dict vm))
  (write-char #\tab)
  (display i)
  (write-char #\tab)
  (write-json (variable-environment vm))
  (newline)
  )

(define (on-error-throw vm)
  (error "Test Failure - exception thrown")
  )

(: run-until-return (-> Bytes simulation-result))
(define (run-until-return bs)
  (let* ([ result null ]
         [ reverse-symbol-table (invert-dict (*symbol-table*)) ]
         [ on-simulate (if (verbose? VERBOSITY-LOW)
                           (λ (vm i reads) (on-simulate-debug reverse-symbol-table vm i reads))
                           on-simulate-nop)
                       ]
         [ vm (make-vm bs on-simulate)])
    (*reverse-symbol-table* reverse-symbol-table)
    (with-handlers ([ exn:evm:return? (λ (exn-return) (set! result (exn:evm:return-result exn-return)))])
      (simulate! vm MAX-ITERATIONS))
    (simulation-result vm result)))

(: run-test (-> String Pyramid Any))
(define (run-test name prog)
  (with-handlers ([exn:evm? (λ (x) x)]
                  ;; [exn:fail? (λ (x)
                  ;;              (begin
                  ;;                (displayln `("Unexpected exception encountered" ,x))
                  ;;                x)))
                  )
                               
    (*include-directory* "tests")
    (let* ([ params (full-compile prog) ]
           [ initializer-bs (third params) ]
           [ deploy-result (run-until-return initializer-bs) ]
           [ program-bs (simulation-result-val deploy-result) ]
           [ exec-result (run-until-return program-bs) ]
           )
      (cons deploy-result exec-result))))

(define (evm-result-equal? a b)
  (cond [(and (exn? a) (exn? b)) (equal? (exn-message a) (exn-message b)) ]
        [else (equal? a b)]))

(: minimize-test (-> String Pyramid Pyramid))
(define (minimize-test name prog)
  (let* ([ baseline (run-test name prog) ]
         [ pred? (λ (candidate)
                   (let ([ result (run-test name candidate) ])
                     (begin
                       ; (debug-print `(,candidate ,baseline ,result))
                       (evm-result-equal? baseline result))))]
         [ result (minimize pred? prog) ]
         )
    (pretty-print result)))

(: debug-info (-> (Pair simulation-result simulation-result) Any))
(define (debug-info result)
  (let* ([ deploy-result (car result) ]
         [ exec-result (cdr result) ]
         [ exec-vm (simulation-result-vm exec-result) ]
         [ exec-val (simulation-result-val exec-result) ]
         [ deploy-vm (simulation-result-vm deploy-result) ]
         [ deploy-val (simulation-result-val deploy-result) ]
         )
    (list
     (cons 'es (vm-exec-step exec-vm))
     (cons 'eg (vm-exec-gas exec-vm))
     (cons 'ez (bytes-length (vm-exec-bytecode exec-vm)))
     (cons 'ds (vm-exec-step deploy-vm))
     (cons 'dg (vm-exec-gas deploy-vm))
     (cons 'dz (bytes-length (vm-exec-bytecode deploy-vm))))))


; A test is a regular Pyramid program that uses special test macros to communicate with the compiler.
(: assert-test (-> String Pyramid Void))
(define (assert-test name prog)
  (if (*minimize?*)
      (minimize-test name prog)
      (let* ([ result      (run-test name prog) ])
        (assert-equal-vm name
                         (*test-expected-result*)
                         (simulation-result-val (cdr result))
                         (debug-info result)))))

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

