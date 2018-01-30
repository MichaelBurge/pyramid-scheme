#lang typed/racket

; (require test-engine/racket-tests)
;(require racket/cmdline)

(require "types.rkt")
(require "ast.rkt")
(require "compiler.rkt")
(require "io.rkt")
(require "codegen.rkt")
(require "serializer.rkt")
(require "disassembler.rkt")
(require "utils.rkt")
(require "simulator.rkt")
(require "analysis.rkt")
(require (except-in "macro.rkt" make-label))
(require "globals.rkt")
;(require "minimize.rkt")
(require "storage.rkt")
(require "accessors.rkt")
(require "abi.rkt")
(require "transaction.rkt")
(require "wallet.rkt")

(require "typed/binaryio.rkt")

(require/typed "simulator.rkt"
  [ make-simulator (-> simulator)]
  [ apply-txn-create! (-> simulator vm-txn simulation-result-ex)]
  [ apply-txn-message! (-> simulator vm-txn simulation-result-ex)]
  [ mint-ether! (-> simulator Symbol EthWord Void)]
  [ *on-simulate-instruction* (Parameterof OnSimulateCallback)]
  [ on-simulate-debug (-> ReverseSymbolTable OnSimulateCallback)]
  [ on-simulate-nop   OnSimulateCallback ]
  )

(provide (all-defined-out))

(: assert-equal (-> String Any Any Any Void))
(define (assert-equal name expected actual debug-info)
  (if (equal? expected actual)
      (begin
        (display `("Test Passed: " ,name ,expected ,actual ,debug-info))
        (newline))
      (error "Test failed: " name expected actual))
  )

(: assert-expectation (-> simulation-result-ex test-expectation Void))
(define (assert-expectation simres expect)
  (if (simulation-result? simres)
      (assert-equal (test-expectation-name expect)
                    (test-expectation-expected expect)
                    ((test-expectation-actual expect) simres)
                    (debug-info simres))
      (error "Test failed: " (test-expectation-name expect) simres)))

;; (define (assert-equal-vm name expected actual-bs debug-info)
;;   (if (exn:fail? actual-bs)
;;       (error "assert-equal-vm: Caught error" actual-bs)
;;       (let ([ actual (parse-type (infer-type expected) actual-bs) ])
;;         (assert-equal name expected actual debug-info))))

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
      (*on-simulate-instruction* (on-simulate-debug (invert-hash (*symbol-table*)))))
    (let* ([ params         (full-compile prog) ]
           [ initializer-bs (full-compile-result-bytes params)]
           [ sim            (make-simulator)]
           [ deploy-txn     (make-txn-create initializer-bs)]
           [ deploy-result  (cast (apply-txn-create! sim deploy-txn) simulation-result)]
           [ contract       (vm-txn-receipt-contract-address (simulation-result-txn-receipt deploy-result))]
           [ program-txn    (make-txn-message (cast contract Address) 0 (bytes)) ]
           [ exec-result    (apply-txn-message! sim program-txn)]
           )
      (cons deploy-result exec-result)))

(define (evm-result-equal? a b)
  (cond [(and (exn? a) (exn? b)) (equal? (exn-message a) (exn-message b)) ]
        [else (equal? a b)]))

;; (: minimize-test (-> String Pyramid Pyramid))
;; (define (minimize-test name prog)
;;   (let* ([ baseline (run-test name prog) ]
;;          [ pred? (λ (candidate)
;;                    (let ([ result (run-test name candidate) ])
;;                      (begin
;;                        ; (debug-print `(,candidate ,baseline ,result))
;;                        (evm-result-equal? baseline result))))]
;;          [ result (minimize pred? prog) ]
;;          )
;;     (pretty-print result)))

(: debug-info (-> simulation-result Any))
(define (debug-info result)
  (let* ([ exec-vm (simulation-result-vm result) ]
         [ exec-val (simulation-result-val result) ]
         )
    (list
     (cons 's (vm-exec-step exec-vm))
     (cons 'g (vm-exec-gas exec-vm))
     (cons 'z (bytes-length (vm-exec-bytecode exec-vm))))))

(: run-test-case (-> String Bytes test-case simulation-result-exs))
(define (run-test-case name bytecode cs)
  (when (verbose? VERBOSITY-LOW)
    (*on-simulate-instruction* (on-simulate-debug (invert-hash (*symbol-table*)))))
  (let* ([ sim            (make-simulator)]
         [ deploy-txn     (second (test-case-deploy-txn->vm-txn cs bytecode))]
         [ deploy-result  (cast (apply-txn-create! sim deploy-txn) simulation-result)]
         [ contract?      (vm-txn-receipt-contract-address (simulation-result-txn-receipt deploy-result))]
         [ contract       (if (null? contract?) (error "Unable to deploy contract" deploy-result) contract?)]
         [ expect-txns    (test-case-msg-txns->vm-txns cs contract)]
         )
    (verbose-section "Deployed" VERBOSITY-HIGH
                     (λ ()
                       (define bs (simulation-result-val deploy-result))
                       (print-disassembly bs)
                       ))
    (register-addr-name! 'contract contract)
    (mint-accounts! sim cs)
    (cons deploy-result
          (for/list : simulation-result-exs
              ([ expect-txn : (List test-expectations vm-txn) expect-txns ])
            (match expect-txn
              [(list expects msg-txn)
               (force-txn-sender! msg-txn 'sender)
               (find-or-create-addr-name! 'sender)
               (let* ([ exec-result (apply-txn-message! sim msg-txn)])
                 (for ([ expect : test-expectation expects ])
                   (assert-expectation exec-result expect))
                 exec-result
                 )]
              [x (error "run-test-case: Unexpected item" x)])))))
  
(: run-test-suite (-> String Bytes test-suite Void))
(define (run-test-suite name bytecode suite)
  (for ([ x (test-suite-cases suite) ])
    (run-test-case name bytecode x)
    ))

; A test is a regular Pyramid program that uses special test macros to communicate with the compiler.
(: assert-test (-> String PyramidQ Void))
(define (assert-test name prog)
  (*include-directory* "tests")
  ;; (if (*minimize?*)
  ;;     (minimize-test name prog)
      (let ([ result (full-compile prog) ])
        (run-test-suite name (full-compile-result-bytes result) (*test-suite*))))

(: test-case-deploy-txn->vm-txn (-> test-case Bytes (List test-expectations vm-txn)))
(define (test-case-deploy-txn->vm-txn cs bs)
  (: expectations test-expectations)
  (define expectations null)
  (: txn vm-txn)
  (define txn (make-txn-create bs))
  (set! expectations
        (append expectations
                (apply-modifiers! (test-txn-mods (test-case-deploy-txn cs))
                                  apply-init-modifier!
                                  txn)))
  (list expectations txn))

(: test-case-msg-txns->vm-txns (-> test-case Address (Listof (List test-expectations vm-txn))))
(define (test-case-msg-txns->vm-txns cs addr)
  (: ttxns test-txns)
  (define ttxns (test-case-msg-txns cs))
  (: make-msg-txn (-> test-txn (List test-expectations vm-txn)))
  (define (make-msg-txn ttxn)
    (: expectations test-expectations)
    (define expectations null)
    (: txn vm-txn)
    (define txn (make-txn-message addr 0 (bytes)))
    (set! expectations
          (append expectations
                  (apply-modifiers! (test-txn-mods ttxn) apply-txn-modifier! txn)))
    (list expectations txn))
  (map make-msg-txn ttxns))

(: apply-modifiers! (All (A B) (-> PyramidQs (-> PyramidQ A (Listof B)) A (Listof B))))
(define (apply-modifiers! xs f! acc)
  (: ret (Listof (Listof B)))
  (define ret (map (λ ([ x : PyramidQ ])
                     (f! x acc))
                   xs))
  (apply append ret)
  )

(: apply-init-modifier! (-> PyramidQ vm-txn test-expectations))
(define (apply-init-modifier! exp txn)
  (match exp
    [(list 'value val)
     (begin (assert val exact-integer?)
            (set-vm-txn-value! txn val)
            null)]
    [(list 'sender (list 'quote name))
     (begin (assert name symbol?)
            (force-txn-sender! txn name)
            null)]
    [_ (error "apply-init-modifier: Unexpected syntax" exp)]))

(: apply-txn-modifier! (-> PyramidQ vm-txn test-expectations))
(define (apply-txn-modifier! exp txn)
  (match exp
    [(list 'value val)
     (begin (assert val exact-integer?)
            (set-vm-txn-value! txn val)
            null)]
    [(list 'sender (list 'quote name))
     (begin (assert name symbol?)
            (force-txn-sender! txn name)
            null)]
    [(list 'data (list 'sender (list 'quote name)))
     (begin (assert name symbol?)
            (let ([ addr (find-name name)])
              (set-vm-txn-data! txn (integer->bytes addr 32 #f #t)))
            null)]
    [(list 'assert-balance (list 'quote addr-name) val)
     (begin (assert val exact-integer?)
            (assert addr-name symbol?)
            (list (expectation-account-value addr-name val)))]
    [(list 'assert-return expected)
     (list (expectation-result expected))]
    [_ (error "apply-txn-modifier: Unexpected syntax" exp)]))

(: add-test-expectation! (-> test-txn test-expectation Void))
(define (add-test-expectation! txn expect)
  (set-test-txn-tests! txn (cons expect (test-txn-tests txn))))


(: expectation-result (-> Any test-expectation))
(define (expectation-result expected)
  (test-expectation "return" expected (make-parser expected)))

(: expectation-account-value (-> Symbol Integer test-expectation))
(define (expectation-account-value sym expected)
  (test-expectation (string-append (symbol->string sym) " account value(wei)")
                    expected
                    (λ (res)
                      (if (simulation-result? res)
                          (simres-account-balance res (find-name sym))
                          res))))

(: mint-accounts! (-> simulator test-case Void))
(define (mint-accounts! sim cs)
  (for ([ acc : test-account (test-case-accounts cs)])
    (debug-print 'mint-accounts! acc)
    (mint-ether! sim
                 (test-account-name acc)
                 (test-account-balance acc))))

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

