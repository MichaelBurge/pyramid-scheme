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

