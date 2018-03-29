#lang typed/racket

(require (submod "types.rkt" simulator))
(require (submod "types.rkt" test))
(require "ast.rkt")
(require "codegen.rkt")
(require "io.rkt")
(require "parser.rkt")
(require "globals.rkt")
(require "simulator.rkt")
(require "crypto.rkt")
(require "utils.rkt")
(require "wallet.rkt")
(require "transaction.rkt")
(require "abi.rkt")
(require "compiler.rkt")
(require racket/match)

(require (submod "typed.rkt" binaryio))

(provide (all-defined-out)
         make-label
         make-label-name
         *assumed-label-size*
         label-name
         maybe->list)

#|
This module is required into the namespace used to evaluate Pyramid macros.

Functions defined here are available to Pyramid programs within macros.
|#

(define (%-install-macro-library!)
  (: base-ns Namespace)
  (define base-ns (current-namespace))
  (: attach (-> Module-Path Any))
  (define (attach m)
    (define ns (*macro-namespace*))
    (namespace-attach-module base-ns m ns)
    (parameterize ([ current-namespace ns ])
      (namespace-require m)))
  (attach "globals.rkt")
  (attach "types.rkt")
  (attach 'racket/list)
  (attach "macro.rkt")
  (attach "ast.rkt")
  (attach "utils.rkt")
  (attach 'racket/match)

  ; Debug tools
  (attach "io.rkt")
  (attach 'racket/pretty)

  (install-macro! 'include %-include)
  (install-macro! 'require %-require)
  (install-macro! 'test-suite %-test-suite)
  (install-macro! 'set-test-result! %-test-result)
  )

; Compiles a fragment of code rather than a whole program.
; The fragment doesn't have standard library or environment initialization code.
(: minicompile (-> PyramidQ EthInstructions))
(define (minicompile prog)
  (codegen-list (inst-seq-statements (compile-pyramid 'val 'next (expand-pyramid prog)))))

(: contains-instruction? (-> Pyramid (-> EthInstruction Boolean) Boolean))
(define (contains-instruction? prog pred)
  (let ((is (minicompile prog)))
    (ormap pred is)))

(: accesses-memory? (-> Pyramid Boolean))
(define (accesses-memory? exp)
  (define (read? i) (equal? i (evm-op 'MLOAD)))
  (define (write? i) (equal? i (evm-op 'MSTORE)))
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

(: %-test-suite (-> PyramidQ * PyramidQ))
(define (%-test-suite . exps)
  (set-test-suite! (make-test-suite exps))
  '(begin))

(: %-test-result (-> PyramidQ PyramidQ))
(define (%-test-result exp)
  (set-test-suite! (make-simple-test-suite exp))
  '(begin))

(: make-test-suite (-> PyramidQs test-suite))
(define (make-test-suite exps)
  (: make-test-case (-> String PyramidQs PyramidQ PyramidQs test-case))
  (define (make-test-case name accounts deploy-txn msg-txns)
    (test-case
        name
      (map make-test-account accounts)
      (make-test-deploy deploy-txn)
      (map make-test-txn msg-txns)))

  (test-suite
   "undefined" ; TODO: Use the currently-compiled filename
   (for/list : test-cases ([ exp : PyramidQ exps ])
     (match exp
       [`(case ,name (accounts . ,accounts) ,deploy-txn . ,msg-txns)
        (begin (assert name string?)
               (assert accounts list?)
               (assert msg-txns list?)
               (make-test-case name accounts deploy-txn msg-txns))]
       [`(case ,name ,deploy-txn . ,msg-txns)
        (begin (assert name string?)
               (assert msg-txns list?)
               (make-test-case name '() deploy-txn msg-txns))]
       [_ (error "make-test-suite: Unexpected syntax" exp)]))))

(: make-simple-test-suite (-> PyramidQ test-suite))
(define (make-simple-test-suite expected)
  (test-suite
   "undefined" ; TODO: Use the currently-compiled filename
   (listof (test-case "undefined"
             '()
             (test-txn null null)
             (list (test-txn (list `(assert-return ,expected))
                             (list (test-expectation "assert-return" expected (make-parser expected)))))))))

(: make-test-account (-> PyramidQ test-account))
(define (make-test-account exp)
  (match exp
    [`((quote ,(? symbol? name)) ,(? exact-nonnegative-integer? balance)) (test-account name balance)]
    [ _ (error "make-test-account: Unknown syntax" exp)]))

(: make-test-deploy (-> PyramidQ test-txn))
(define (make-test-deploy exp)
  (match exp
    [(list 'init modifiers ...) (test-txn modifiers null)]
    [x (error "make-test-deploy: Unexpected syntax" x)]))

(: make-test-txn (-> PyramidQ test-txn))
(define (make-test-txn exp)
  (match exp
    [(list 'txn modifiers ...) (test-txn modifiers null)]
    [_ (error "make-test-txn: Unexpected syntax" exp)]
    ))

(: make-parser (-> Any (-> simulation-result-ex Any)))
(define (make-parser expected)
  (λ (x)
    (if (simulation-result? x)
        (parse-type (infer-type expected) (simulation-result-val x))
        x)))

;; (define (%-selector sig) (keccak-256 (string->bytes/utf-8 (%-sig-str sig))))

(: macro-read-file (-> String PyramidQ))
(define (macro-read-file path)
  (read-file path #:execute? #f))

(: %-include (case-> (-> String Any) (-> Symbol String Any)))
(define %-include
  (case-lambda
    ([mod] (parameterize ([ current-directory (*include-directory*) ])
             (macro-read-file mod)))
    ([collection mod] (parameterize ([ current-directory (get-collection-directory collection) ])
                        (macro-read-file mod)))
    ))

(: include-unless-cached (-> Symbol String Any))
(define (include-unless-cached collection mod)
  (let* ([ key (string-append (symbol->string collection) "/" mod) ]
         [ has-key? (set-member? (*required-modules*) key)]
         [ add-key! (λ () (*required-modules* (set-add (*required-modules*) key)))])
    (if has-key?
        '(begin)
        (begin (add-key!)
               (if (equal? collection '*current-directory)
                   (%-include mod)
                   (%-include collection mod))))))

(: %-require (case-> (-> String Any) (-> Symbol String Any)))
(define %-require
  (case-lambda
    ([ mod ] (include-unless-cached '*current-directory mod))
    ([ collection mod ]
     (assert collection symbol?)
     (assert mod string?)
     (include-unless-cached collection mod))))

; A Patchpoint consists of a symbol table entry that should be replaced with the stack output of some bytecode.
(: %-register-patchpoint! (-> Symbol EthInstructions Void))
(define (%-register-patchpoint! sym ethis)
  (define pp (patchpoint sym ethis))
  (*patchpoints* (cons pp (*patchpoints*))))
