#lang typed/racket/no-check

; "Analysis" refers to the layer that handles the full compilation pipeline from Pyramid to Bytecode.

(require racket/pretty)

(require "types.rkt")
(require "ast.rkt")
(require "compiler.rkt")
(require "codegen.rkt")
(require "disassembler.rkt")
(require "serializer.rkt")
(require "macro.rkt")
(require "globals.rkt")
(require "io.rkt")

(provide (all-defined-out))

(define *link?* (make-parameter #t))

(: maybe-link (-> Bytes Bytes))
(define (maybe-link bs)
  (if (*link?*)
      (wrap-loader bs)
      bs))

;; (: reinitialize-globals! (-> Void))
;; (define (reinitialize-globals!)
;;   (*available-macros* (make-empty-namespace))
;;   (set! label-counter 0)
;;   (*macro-namespace* (make-base-namespace))
;;   (%-install-macro-library)
;;   (reset-serializer-globals!)
;;   )

(define (verbose-section title body)
  (when (*verbose?*)
    (display title)
    (newline) (body)
    (newline)))

(: full-compile (-> Pyramid (List Instructions EthInstructions Bytes)))
(define (full-compile prog)
  ; (reinitialize-globals!)
  (%-install-macro-library)
  (verbose-section "Program"
                   (λ () (pretty-print prog)))
  (let ([ instructions     (compile-pyramid prog 'val 'next) ])
    (verbose-section "Abstract Machine Code"
                     (λ () (display-all (inst-seq-statements instructions))))
    (let ([ eth-instructions (codegen (inst-seq-statements instructions)) ])
      (verbose-section "EVM Instructions"
                       (λ () (display-all eth-instructions)))
      (let ([ bs (maybe-link (serialize-with-relocations eth-instructions)) ])
        (verbose-section "EVM Disassembly"
                         (λ () (print-disassembly bs)))
        (list instructions eth-instructions bs)))))
