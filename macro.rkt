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
       [(list 'case name txns ...) (test-case name (map make-test-txn txns))]
       [_ (error "make-test-suite: Unexpected syntax" exp)]))))

(: make-parser (-> Any (-> simulation-result-ex Any)))
(define (make-parser expected)
  (λ (x)
    (if (simulation-result? x)
        (parse-type (infer-type expected) (simulation-result-val x))
        x)))
  
(: make-test-txn (-> Pyramid (-> test-txn)))
(define (make-test-txn exp)
  (match exp
    [(list 'txn (list 'result expected))
     (λ ()
       (test-txn (make-txn-message (*test-contract*) 0 (bytes))
                 expected
                 (make-parser expected)))
     ]
    [_ (error "make-test-txn: Unexpected syntax" exp)]
    ))

(: make-simple-test-suite (-> Pyramid Void))
(define (make-simple-test-suite exp)
  (test-suite
   "undefined" ; TODO: Use the currently-compiled filename
   (list (test-case "undefined"
           (match exp
             [ expected (list (λ ()
                                (test-txn (make-txn-message (*test-contract*) 0 (bytes))
                                          expected
                                          (make-parser expected))))])
           ))))

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
  

