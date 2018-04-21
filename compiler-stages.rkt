#lang typed/racket

(require racket/pretty)

(require "abstract-analyzer.rkt")
(require "ast.rkt")
(require "compiler.rkt")
(require "codegen.rkt")
(require "disassembler.rkt")
(require "serializer.rkt")
(require (except-in "macro.rkt" make-label))
(require "globals.rkt")
(require "io.rkt")
; (require "minimize.rkt")
(require "simplifier.rkt")
(require (submod "expander.rkt" typed))
(require (submod "types.rkt" ast))
(require (submod "types.rkt" pyramidc))

(provide (all-defined-out))

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

;; (: minimize-pred (-> Pyramid (-> Pyramid Boolean) Pyramid))
;; (define (minimize-pred exp pred?)
;;   (let ([ next (memf pred? (reductions exp)) ])
;;     (if (equal? next #f)
;;         exp
;;         next)))

(: print-program-settings (-> Void))
(define (print-program-settings)
  (define pps (*patchpoints*))
  (unless (null? pps)
    (displayln "Patch Points")
    (for ([ pp pps ])
      (displayln pp))))

(: sort-source-map (-> SourceMap (Listof (Pairof Symbol Symbols))))
(define (sort-source-map map)
  ((inst sort (Pairof Symbol Symbols)) (hash->list map) symbol<? #:key car))

(: print-source-map (-> SourceMap Void))
(define (print-source-map map)
  (for ([ p (sort-source-map map)])
    (printf "~a\t~a\n" (car p) (cdr p))))

(: full-compile (-> Syntax full-compile-result))
(define (full-compile prog)
  ; (reinitialize-globals!)
  (verbose-section "Program" VERBOSITY-LOW
                   (pretty-print (syntax->datum prog)))
  (let ([ expanded (expand-pyramid prog)])
    (verbose-section "Expanded" VERBOSITY-MEDIUM
                     (pretty-print expanded))
    (verbose-section "Reshrunk" VERBOSITY-HIGH
                     (print-ast expanded))
    (let ([ simplified (if (*simplify?*) (simplify expanded) expanded) ])
      (verbose-section "Simplified" VERBOSITY-LOW
                       (print-ast simplified))
      (verbose-section "Settings" VERBOSITY-LOW
                       (print-program-settings))
      (let ([ instructions     (compile-pyramid 'val 'next simplified) ])
        (verbose-section "Abstract Machine Code" VERBOSITY-LOW
                         (display-abstract-instructions instructions))
        (verbose-section "Abstract Simulation" VERBOSITY-LOW (begin))
        (run-instructions (inst-seq-statements instructions))
        (let ([ eth-instructions (codegen (inst-seq-statements instructions)) ])
          (verbose-section "EVM Instructions" VERBOSITY-HIGH
                           (display-all eth-instructions))
          (let ([ bs-unlinked (serialize-with-relocations eth-instructions) ])
            (let ([ bs (maybe-link bs-unlinked) ])
              (verbose-section "Symbol Table" VERBOSITY-MEDIUM
                               (print-symbol-table (*symbol-table*)))
              (verbose-section "Source Maps" VERBOSITY-HIGH
                               (print-source-map (*evm-source-map*)))
              (verbose-section "EVM Disassembly" VERBOSITY-LOW
                               (print-disassembly bs))

              ;(λ () (print-disassembly bs-unlinked)))
              (full-compile-result bs (inst-seq-statements instructions) eth-instructions)
              )))))))
