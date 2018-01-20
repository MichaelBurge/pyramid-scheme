#lang errortrace typed/racket/no-check

; (require test-engine/racket-tests)
(require racket/cmdline)
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
(require "storage.rkt")

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

(define (on-error-throw vm)
  (error "Test Failure - exception thrown")
  )
 
(: run-test (-> String Pyramid Any))
(define (run-test name prog)
  ;; (with-handlers ([exn:evm? (λ (x) x)]
  ;;                 ;; [exn:fail? (λ (x)
  ;;                 ;;              (begin
  ;;                 ;;                (displayln `("Unexpected exception encountered" ,x))
  ;;                 ;;                x)))
  ;;                 )
    (*include-directory* "tests")
    (when (verbose? VERBOSITY-LOW)
      (*on-simulate-instruction* (on-simulate-debug (invert-dict (*symbol-table*)))))
    (let* ([ params         (full-compile prog) ]
           [ initializer-bs (third params) ]
           [ sim            (simulator (vm-world (make-hash)) (make-store))]
           [ deploy-txn     (make-txn-create initializer-bs)]
           [ deploy-result  (apply-txn-create! sim deploy-txn)]
           [ contract       (vm-txn-receipt-contract-address (simulation-result-txn-receipt deploy-result))]
           [ program-txn    (make-txn-message contract 0 (bytes)) ]
           [ exec-result    (apply-txn-message! sim program-txn)]
           )
      (cons deploy-result exec-result)))

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

(: run-test-case (-> Bytes test-case simulation-result-exs))
(define (run-test-case bytecode cs)
  (*include-directory* "tests")
  (when (verbose? VERBOSITY-LOW)
    (*on-simulate-instruction* (on-simulate-debug (invert-dict (*symbol-table*)))))
  (let* ([ sim            (simulator (vm-world (make-hash)) (make-store))]
         [ deploy-txn     (make-txn-create bytecode)]
         [ deploy-result  (apply-txn-create! sim deploy-txn)]
         [ contract       (vm-txn-receipt-contract-address (simulation-result-txn-receipt deploy-result))]
         [ program-txn    (make-txn-message contract 0 (bytes)) ]
         [ exec-result    (apply-txn-message! sim program-txn)]
         )
    (cons deploy-result
          (for/list ([ txn-test (test-case-txns cs) ]
                     [ i (length (test-case-txns cs)) ])
            (let ([ exec-result (apply-txn-message! sim (test-txn-txn txn-test))]
                  [ name (string-append (test-case-name cs) "/" (integer->string i))])
              (assert-equal name (test-txn-expected txn-test (test-txn-actual exec-result)))
              )))))

(: run-test-suite (-> Bytes test-suite Void))
(define (run-test-suite bytecode suite)
  (for ([ x (test-suite-cases suite) ])
    (run-test-case bytecode x)
    ))

; A test is a regular Pyramid program that uses special test macros to communicate with the compiler.
(: assert-test (-> String Pyramid Void))
(define (assert-test name prog)
  (if (*minimize?*)
      (minimize-test name prog)
      (let ([ bytecode (full-compile prog) ])
        (displayln (*test-suite*))
        (run-test-suite bytecode (*test-suite*)))))

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

