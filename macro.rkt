#lang typed/racket/no-check

(require "types.rkt")
(require "ast.rkt")
(require "compiler.rkt")
(require "codegen.rkt")
(require "io.rkt")
(require "parser.rkt")
(require "globals.rkt")
(require "simulator.rkt")
(require racket/match)

(provide (all-defined-out))
#|
This module is required into the namespace used to evaluate Pyramid macros.

Functions defined here are available to Pyramid programs within macros.
|#

(define (%-install-macro-library)
  (parameterize ([ current-namespace (*macro-namespace*) ])
    (namespace-require 'racket/list)
    (namespace-require "macro.rkt")
    (namespace-require "ast.rkt")
    )
  (install-macro! 'include %-include)
  (install-macro! 'require %-require)
  )

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

; Example: $ echo -n 'baz(uint32,bool)' | keccak-256sum
(define (%-selector sig)
  (with-input-from-string (%-sig-str sig)
    (lambda ()
      (with-output-to-string
        (lambda ()
          (system "keccak-256sum"))))))

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
  (displayln (*test-suite*))
  )

(: make-test-suite (-> Pyramids Void))
(define (make-test-suite exps)
  (test-suite
   "undefined" ; TODO: Use the currently-compiled filename
   (for/list ([ exp exps ])
     (match exp
       [(list 'case name txns ...) (test-case name (map make-test-txn txns))]
       [_ (error "make-test-suite: Unexpected syntax" exp)]))))

(: make-parser (-> Any (-> simulation-result-ex Any)))
(define (make-parser expected)
  (λ (x)
    (if (exn:evm:return? x)
        (parse-type (infer-type expected) (exn:evm:return-result x))
        x)))
  
(: make-test-txn (-> Pyramid test-txn))
(define (make-test-txn exp)
  (match exp
    [(list 'txn (list 'result expected)) (test-txn (make-txn-message *test-contract* 0 (bytes))
                                              expected
                                              (make-parser expected))]
    [_ (error "make-test-txn: Unexpected syntax" exp)]
    ))

(: make-simple-test-suite (-> Pyramid Void))
(define (make-simple-test-suite exp)
  (match exp
    [(list expected) (test-txn (make-txn-message *test-contract* 0 (bytes))
                               expected
                               (make-parser expected))]
    ))

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
  

