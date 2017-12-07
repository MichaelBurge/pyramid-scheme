#lang typed/racket/no-check

(require "types.rkt")
(require "ast.rkt")
(require "compiler.rkt")
(require "codegen.rkt")

(provide (all-defined-out))

#|
This module is required into the namespace used to evaluate Pyramid macros.

Functions defined here are available to Pyramid programs.
|#

(: accesses-memory? (-> Pyramid Boolean))
(define (accesses-memory? exp) #f)
