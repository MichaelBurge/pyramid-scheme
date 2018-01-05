#lang typed/racket/no-check

; "Analysis" refers to the layer that handles the full compilation pipeline from Pyramid to Bytecode.

(require "types.rkt")
(require "ast.rkt")
(require "compiler.rkt")
(require "codegen.rkt")
(require "serializer.rkt")
(require "macro.rkt")

(provide (all-defined-out))

(define *link?* (make-parameter #t))

(: maybe-link (-> Bytes Bytes))
(define (maybe-link bs)
  (if (*link?*)
      (wrap-loader bs)
      bs))

(: full-compile (-> Pyramid (List Instructions EthInstructions Bytes)))
(define (full-compile prog)
  (%-install-macro-library)
  (let* ([ instructions     (compile-pyramid prog 'val 'next) ]
         [ eth-instructions (codegen (inst-seq-statements instructions)) ]
         [ bs               (maybe-link (serialize-with-relocations eth-instructions)) ])
    (list instructions eth-instructions bs)))
