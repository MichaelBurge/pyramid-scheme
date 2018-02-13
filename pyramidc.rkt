#lang typed/racket

(require racket/cmdline)

(require "ast.rkt")
(require "codegen.rkt")
(require "serializer.rkt")
(require "compiler.rkt")
(require "types.rkt")
(require "io.rkt")
(require "disassembler.rkt")
(require "test.rkt")
(require "analysis.rkt")
(require "globals.rkt")
(require "parser.rkt")
(require (except-in "macro.rkt" make-label))

(require "typed/binaryio.rkt")

(module unsafe typed/racket/no-check
  (require "globals.rkt")
  (provide file-to-compile)
  (: file-to-compile String)
  (define file-to-compile
    (command-line
     #:program "pyramid"
     #:multi
     [("-v" "--verbose") "Compile with verbose messages. Used to debug the compiler."
                         (*verbosity* (+ 1 (*verbosity*)))]
     #:once-each
     [("-g")             "Compile with debug symbols"
                         (*use-debug-symbols?* #t)]
     [("-t")             "Run file as a test"
                         (*test?* #t)]
     [("-m")             "Minify the source"
                         (*minimize?* #t)]
     [("-l" "--link")
      val
      "Generate deployable Ethereum bytecode"
      (*link?* val)]
     #:args (filename) ; expect one command-line argument: <filename>
     filename)))

(require/typed 'unsafe [ file-to-compile String ])

(: verbose-output (-> PyramidQ Void))
(define (verbose-output prog)
  (match (full-compile prog)
    [(struct full-compile-result (bs abstract-is eth-is))
     (begin
       (newline) (display prog)
       (newline) (display "Abstract Instructions:") (newline)
       (display-abstract-instructions abstract-is)
       
       ; (newline) (display "EVM Instructions:") (newline) (display-all eth-is)
       ; (print-symbol-table (*symbol-table*))
       (print-relocations (*relocation-table*))
       (print-disassembly bs))]
    ))
  
(: standard-output (-> PyramidQ Void))
(define (standard-output prog)
  (let ([bs (full-compile-result-bytes (full-compile prog))])
    (write (bytes->hex-string bs))
    (newline)))

(: main (-> Void))
(define (main)
  (%-install-macro-library!)
  (let ([ prog (read-file file-to-compile) ])
    (cond ((*test?*)    (assert-test file-to-compile prog))
          ((verbose? 1) (verbose-output prog))
          (else         (standard-output prog)))
    (when (verbose? VERBOSITY-MEDIUM)
      (display-macros))
    ))

(main)
