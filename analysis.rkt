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
(require "minimize.rkt")
(require "simplifier.rkt")

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

(: minimize-pred (-> Pyramid (-> Pyramid Boolean) Pyramid))
(define (minimize-pred exp pred?)
  (let ([ next (memf pred? (reductions exp)) ])
    (if (equal? next #f)
        exp
        next)))

(define (verbose-section title level body)
  (when (verbose? level)
    (display title)
    (newline) (body)
    (newline)))

(define (print-program-settings)
  (define pps (*patchpoints*))
  (unless (null? pps)
    (displayln "Patch Points")
    (for ([ pp pps ])
      (displayln pp))))

(: full-compile (-> Pyramid (List Instructions EthInstructions Bytes)))
(define (full-compile prog)
  ; (reinitialize-globals!)
  (%-install-macro-library)
  (verbose-section "Program" VERBOSITY-LOW
                   (λ () (pretty-print prog)))
  (let ([ simplified (if (*simplify?*) (simplify prog) prog) ])
    (verbose-section "Simplified Program" VERBOSITY-LOW
                     (λ () (pretty-print simplified)))
    (verbose-section "Program Settings" VERBOSITY-LOW
                     (λ () (print-program-settings)))
    (let ([ instructions     (compile-pyramid simplified 'val 'next) ])
      (verbose-section "Abstract Machine Code" VERBOSITY-LOW
                       (λ () (display-abstract-instructions instructions)))
      (let ([ eth-instructions (codegen (inst-seq-statements instructions)) ])
        (verbose-section "EVM Instructions" VERBOSITY-HIGH
                         (λ () (display-all eth-instructions)))
        (let* ([ bs-unlinked (serialize-with-relocations eth-instructions) ]
               [ bs (maybe-link bs-unlinked) ])
          (verbose-section "Symbol Table" VERBOSITY-MEDIUM
                           (λ () (print-symbol-table (*symbol-table*))))
          (verbose-section "Relocation Table" VERBOSITY-MEDIUM
                           (λ () (print-relocations (*relocation-table*))))
          (verbose-section "EVM Disassembly" VERBOSITY-LOW
                           (λ () (print-disassembly bs)))
                           ;(λ () (print-disassembly bs-unlinked)))
          (full-compile-result bs instructions eth-instructions)
          )))))
