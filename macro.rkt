#lang typed/racket

(require "types.rkt")
(require "ast.rkt")
(require "codegen.rkt")
(require "io.rkt")
(require "parser.rkt")
(require "globals.rkt")
(require "simulator.rkt")
(require "crypto.rkt")
(require "utils.rkt")
(require "accessors.rkt")
(require "wallet.rkt")
(require "transaction.rkt")
(require "abi.rkt")
(require racket/match)

(require "typed/binaryio.rkt")

(require/typed "compiler.rkt"
  [ compile-pyramid (-> Pyramid Target Linkage inst-seq)]
  [ make-label (-> Symbol Fixnum LabelName) ]
  )

(require/typed "codegen.rkt"
  [ codegen-list (Generator Instructions)])

(require/typed "ast.rkt"
  [ install-macro! (-> Symbol Procedure Void)]
  [ expand-macro (-> pyr-macro-application Pyramid)])


(provide (all-defined-out)
         make-label
         label-name)

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

;; (define (%-sig-str sig)
;;   (let ([ name (second sig) ]
;;         [ types (fourth sig) ])
;;     (string-append
;;      name
;;      "("
;;      (string-join (map symbol->string types) ",")
;;      ")")))

;; (define (%-parse-types tys)
;;   (let* ([ os 4 ]
;;          [ parse-ty (lambda ()
;;                       (let ([ ret `(parse-fixnum ,os) ])
;;                         (set! os (+ os 32))
;;                         ret))])
;;     (map parse-ty tys)))

;; (: %-register-export 
;; (define (%-register-export sig)
;;   (*exports* (cons sig (*exports*))))

(: set-test-suite! (-> test-suite Void))
(define (set-test-suite! suite)
  (*test-suite* suite)
  )

(: %-test-suite (-> Pyramid * Pyramid))
(define (%-test-suite . exps)
  (set-test-suite! (make-test-suite exps))
  (pyr-begin '()))

(: %-test-result (-> Pyramid Pyramid))
(define (%-test-result exp)
  (set-test-suite! (make-simple-test-suite exp))
  (pyr-begin '()))

(: make-test-suite (-> Pyramids test-suite))
(define (make-test-suite exps)
  (test-suite
   "undefined" ; TODO: Use the currently-compiled filename
   (for/list ([ exp : Pyramid exps ])
     (match exp
       [(list 'case name deploy-txn txns ...)
        (begin (assert name string?)
               (test-case name (make-test-deploy deploy-txn) (map make-test-txn txns)))]
       [_ (error "make-test-suite: Unexpected syntax" exp)]))))

(: make-parser (-> Any (-> simulation-result-ex Any)))
(define (make-parser expected)
  (λ (x)
    (if (simulation-result? x)
        (parse-type (infer-type expected) (simulation-result-val x))
        x)))

(: make-test-deploy (-> Pyramid (-> Bytes test-txn)))
(define (make-test-deploy exp)
  (match exp
    [(list 'init modifiers ...)
     (λ ([ bs : Bytes ])
       (apply-modifiers! modifiers
                         apply-init-modifier!
                         (test-txn (delay (make-txn-create bs))
                                   null)))]
    [x (error "make-test-deploy: Unexpected syntax" x)]))

  
(: make-test-txn (-> Pyramid (-> Address test-txn)))
(define (make-test-txn exp)
  (match exp
    [(list 'txn modifiers ...)
           (λ ([ contract : Address ])
             (apply-modifiers! modifiers
                               apply-txn-modifier!
                               (test-txn (delay (make-txn-message contract 0 (bytes)))
                                         null
                                         )))]
    [_ (error "make-test-txn: Unexpected syntax" exp)]
    ))

(: apply-modifiers! (All (A) (-> Pyramids (-> Pyramid A Void) A A)))
(define (apply-modifiers! xs f! acc)
  (for ([ x xs])
    (f! x acc))
  acc)

(: apply-init-modifier! (-> Pyramid test-txn Void))
(define (apply-init-modifier! exp txn)
  (match exp
    [(list 'value val)
     (begin (assert val exact-integer?)
            (compose-test-txn! txn (λ (txn2) (set-vm-txn-value! txn2 val))))]
    [(list 'sender (list 'quote name))
     (begin (assert name symbol?)
            (compose-test-txn! txn (λ (txn2) (force-txn-sender! txn2 name))))]
    [_ (error "apply-init-modifier: Unexpected syntax" exp)]))

(: apply-txn-modifier! (-> Pyramid test-txn Void))
(define (apply-txn-modifier! exp txn)
  (match exp
    [(list 'value val)
     (begin (assert val exact-integer?)
            (compose-test-txn! txn (λ (txn) (set-vm-txn-value! txn val))))]
    [(list 'sender (list 'quote name))
     (begin (assert name symbol?)
            (compose-test-txn! txn (λ (txn2) (force-txn-sender! txn2 name))))]
    [(list 'data (list 'sender (list 'quote name)))
     (begin (assert name symbol?)
            (compose-test-txn! txn
                               (λ (txn)
                                 (let ([ addr (find-addr-name name)])
                                   (set-vm-txn-data! txn (integer->bytes addr 32 #f #t))))))]
    [(list 'assert-balance (list 'quote addr-name) val)
     (begin (assert val exact-integer?)
            (assert addr-name symbol?)
            (add-test-expectation! txn (expectation-account-value addr-name val)))]
    [(list 'assert-return expected) (add-test-expectation! txn (expectation-result expected))]
    [_ (error "apply-txn-modifier: Unexpected syntax" exp)]))

(: compose-test-txn! (-> test-txn (-> vm-txn Void) Void))
(define (compose-test-txn! txn f!)
  (set-test-txn-txn! txn (delay
                           (let ([ txn2 (force (test-txn-txn txn))])
                             (f! txn2)
                             txn2))))


(: expectation-result (-> Any test-expectation))
(define (expectation-result expected)
  (test-expectation "return" expected (make-parser expected)))

(: expectation-account-value (-> Symbol Integer test-expectation))
(define (expectation-account-value sym expected)
  (test-expectation (string-append (symbol->string sym) " account value(wei)")
                    expected
                    (λ (res)
                      (if (simulation-result? res)
                          (simres-account-balance res (find-addr-name sym))
                          res))))

(: add-test-expectation! (-> test-txn test-expectation Void))
(define (add-test-expectation! txn expect)
  (set-test-txn-tests! txn (cons expect (test-txn-tests txn))))
    
(: make-simple-test-suite (-> Pyramid test-suite))
(define (make-simple-test-suite exp)
  (test-suite
   "undefined" ; TODO: Use the currently-compiled filename
   (listof (test-case "undefined"
             (λ (bs) (test-txn (delay (make-txn-create bs)) '()))
             (match exp
               [ expected (listof (λ ([contract : Address])
                                    (test-txn (delay (make-txn-message contract 0 (bytes)))
                                              (listof (test-expectation "return" expected (make-parser expected))))))]))
           )))

;; (define (%-selector sig) (keccak-256 (string->bytes/utf-8 (%-sig-str sig))))

(: %-include (case-> (-> String Pyramid) (-> Symbol String Pyramid)))
(define %-include
  (case-lambda
    ([mod] (parameterize ([ current-directory (*include-directory*) ])
             (read-file mod)))
    ([collection mod] (parameterize ([ current-directory (get-collection-directory collection) ])
                        (read-file mod)))
    ))

(: %-require (case-> (-> String Pyramid) (-> Symbol String Pyramid)))
(define %-require
  (let ([ with-key-check : (-> Symbol String (Promise Pyramid) Pyramid)
          (λ (collection mod act)
            (let ([ key (string-append (symbol->string collection) "/" mod) ])
              (if (set-member? (*required-modules*) key)
                  (pyr-begin '())
                  (force act))))])
      (case-lambda
        ([ mod ] (with-key-check '*current-directory* mod (delay (%-include mod))))
        ([ collection mod ]
         (assert collection symbol?)
         (assert mod string?)
         (with-key-check collection mod (delay (%-include collection mod)))
         ))))

; A Patchpoint consists of a symbol table entry that should be replaced with the stack output of some bytecode.
(: %-register-patchpoint! (-> Symbol EthInstructions Void))
(define (%-register-patchpoint! sym ethis)
  (define pp (patchpoint sym ethis))
  (*patchpoints* (cons pp (*patchpoints*))))
