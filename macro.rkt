#lang typed/racket/no-check

(require "types.rkt")
(require "ast.rkt")
(require "compiler.rkt")
(require "codegen.rkt")
(require "io.rkt")
(require "parser.rkt")
(require "globals.rkt")
(require "simulator.rkt")
(require "crypto.rkt")
(require "utils.rkt")
(require "accessors.rkt")
(require racket/match)

(provide (all-defined-out)
         make-label
         label-name
         )
#|
This module is required into the namespace used to evaluate Pyramid macros.

Functions defined here are available to Pyramid programs within macros.
|#

(define (%-install-macro-library)
  (define base-ns (current-namespace))
  (parameterize ([ current-namespace (*macro-namespace*) ])
    (namespace-attach-module base-ns "globals.rkt" (current-namespace))
    (namespace-attach-module base-ns "types.rkt" (current-namespace))
    (namespace-require 'racket/list)
    (namespace-require "macro.rkt")
    (namespace-require "ast.rkt") 

    ; Debug tools
    (namespace-require "io.rkt") ; debug-print
    (namespace-require 'racket/pretty)
    )
  (install-macro! 'include %-include)
  (install-macro! 'require %-require)
  (install-macro! 'test-suite %-test-suite)
  (install-macro! 'set-test-result! %-test-result)
  )

(define (snoc xs x) (append xs (list x)))
(define (maybe->list pred? x) (if pred? (list x) '()))

; Compiles a fragment of code rather than a whole program.
; The fragment doesn't have standard library or environment initialization code.
(: minicompile (-> Pyramid EthInstructions))
(define (minicompile prog)
  (codegen-list (inst-seq-statements (compile-pyramid prog 'val 'next))))

(: contains-instruction? (-> Pyramid (-> EthInstruction Boolean) Boolean))
(define (contains-instruction? prog pred)
  (let ((is (minicompile prog)))
    (ormap pred is)))

(: accesses-memory? (-> Pyramid Boolean))
(define (accesses-memory? exp)
  (define (read? i) (equal? i (eth-asm 'MLOAD)))
  (define (write? i) (equal? i (eth-asm 'MSTORE)))
  (define (inst-pred i) (or (read? i)
                            (write? i)))
  (contains-instruction? exp inst-pred))

(define (%-sig-str sig)
  (let ([ name (second sig) ]
        [ types (fourth sig) ])
    (string-append
     name
     "("
     (string-join (map symbol->string types) ",")
     ")")))

(define (%-parse-types tys)
  (let* ([ os 4 ]
         [ parse-ty (lambda ()
                      (let ([ ret `(parse-fixnum ,os) ])
                        (set! os (+ os 32))
                        ret))])
    (map parse-ty tys)))

(define (%-register-export sig)
  (*exports* (cons sig (*exports*))))

(define (set-test-suite! suite)
  (*test-suite* suite)
  )

(define (%-test-suite . exps)
  (set-test-suite! (make-test-suite exps))
  '(begin))

(define (%-test-result exp)
  (set-test-suite! (make-simple-test-suite exp))
  '(begin))

(: make-test-suite (-> Pyramids Void))
(define (make-test-suite exps)
  (test-suite
   "undefined" ; TODO: Use the currently-compiled filename
   (for/list ([ exp exps ])
     (match exp
       [(list 'case name deploy-txn txns ...)
        (test-case name (make-test-deploy deploy-txn) (map make-test-txn txns))]
       [_ (error "make-test-suite: Unexpected syntax" exp)]))))

(: make-parser (-> Any (-> simulation-result-ex Any)))
(define (make-parser expected)
  (λ (x)
    (if (simulation-result? x)
        (parse-type (infer-type expected) (simulation-result-val x))
        x)))

(: make-test-deploy (-> Pyramid test-txn))
(define (make-test-deploy exp)
  (match exp
    [(list 'init modifiers ...)
     (λ (bs)
       (apply-modifiers! modifiers
                         apply-init-modifier!
                         (make-txn-create bs)))]
    [_ (error "make-test-deploy: Unexpected syntax" exp)]))

  
(: make-test-txn (-> Pyramid (-> test-txn)))
(define (make-test-txn exp)
  (match exp
    [(list 'txn modifiers ...)
           (λ (contract)
             (apply-modifiers! modifiers
                               apply-txn-modifier!
                               (test-txn (make-txn-message contract 0 (bytes))
                                         null
                                         )))]
    [_ (error "make-test-txn: Unexpected syntax" exp)]
    ))

(: apply-modifiers! (All (A) (-> Pyramids (-> Pyramid A Void) A Void)))
(define (apply-modifiers! xs f! acc)
  (for ([ x xs])
    (f! x acc))
  acc)

(: apply-init-modifier! (-> Pyramid vm-txn Void))
(define (apply-init-modifier! mod txn)
  (match mod
    [(list 'contract-value val) (set-vm-txn-value! txn val)]
    [_ (error "apply-init-modifier: Unexpected syntax" exp)]))

(: apply-txn-modifier! (-> Pyramid test-txn Void))
(define (apply-txn-modifier! mod txn)
  (match mod
    [(list 'result expected) (add-test-expectation! txn (expectation-result expected))]
    [(list 'txn-value val) (set-vm-txn-value! txn val)]
    [(list 'contract-value val) (add-test-expectation! txn (expectation-contract-value val))]
    [(list 'sender-value val) (add-test-expectation! txn (expectation-sender-value val))]
    [_ (error "apply-txn-modifier: Unexpected syntax" exp)]))

(: expectation-result (-> Any test-expectation))
(define (expectation-result expected)
  (test-expectation "return" expected (make-parser expected)))

(: expectation-contract-value (-> Fixnum test-expectation))
(define (expectation-contract-value expected)
  (test-expectation "contract value(wei)" expected
                    (λ (res) (implies-f simulation-result? simres-contract-value res))))

(: expectation-sender-value (-> Fixnum test-expectation))
(define (expectation-sender-value expected)
  (test-expectation "sender value(wei)" expected
                    (λ (res) (implies-f simulation-result? simres-sender-value res))))

(: add-test-expectation! (-> test-txn test-expectation Void))
(define (add-test-expectation! txn expect)
  (set-test-txn-tests! txn (cons expect (test-txn-tests txn))))
    
(: make-simple-test-suite (-> Pyramid Void))
(define (make-simple-test-suite exp)
  (test-suite
   "undefined" ; TODO: Use the currently-compiled filename
   (list (test-case "undefined"
           (λ (bs) (make-txn-create bs))
           (match exp
             [ expected (list (λ (contract)
                                (test-txn (make-txn-message contract 0 (bytes))
                                          (list (test-expectation "return" expected (make-parser expected))))))]))
         )))

(define (%-selector sig) (keccak-256 (string->bytes/utf-8 (%-sig-str sig))))

(define %-include
  (case-lambda
    ([mod] (parameterize ([ current-directory (*include-directory*) ])
             (read-file mod)))
    ([collection mod] (parameterize ([ current-directory (get-collection-directory collection) ])
                        (read-file mod)))
    ))

(define %-require
  (case-lambda
    ([ mod ] (%-require "*current-directory*" mod))
    ([ collection mod ]
     (let ([ key (string-append (symbol->string collection) "/" mod) ])
       (if (set-member? (*required-modules*) key)
           '(begin)
           (%-include collection mod))))))

; A Patchpoint consists of a symbol table entry that should be replaced with the stack output of some bytecode.
(: %-register-patchpoint! (-> Symbol Bytes Void))
(define (%-register-patchpoint! sym bs)
  (define pp (patchpoint sym bs))
  (*patchpoints* (cons pp (*patchpoints*))))
