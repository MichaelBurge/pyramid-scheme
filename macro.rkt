#lang typed/racket/no-check

(require "types.rkt")
(require "ast.rkt")
(require "compiler.rkt")
(require "codegen.rkt")
(require "io.rkt")

(provide (all-defined-out))
#|
This module is required into the namespace used to evaluate Pyramid macros.

Functions defined here are available to Pyramid programs within macros.
|#

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
          (system "keccak-256sum")))))))

(define (%-parse-types tys)
  (let ([ os 4 ]
        [ parse-ty (lambda ()
                     (let ([ ret `(parse-fixnum ,os) ])
                       (set! os (+ os 32))
                       ret))])
    
    (map parse-ty tys)))

(define (%-register-export sig)
  (set! **exports** (cons sig **exports**)))
