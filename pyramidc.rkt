#lang typed/racket

(require racket/cmdline)

(require "ast.rkt")
(require "serializer.rkt")
(require "compiler.rkt")
(require (submod "types.rkt" ast))
(require (submod "types.rkt" pyramidc))
(require "io.rkt")
(require "disassembler.rkt")
(require "test.rkt")
(require "globals.rkt")
(require "expander.rkt")
(require "loader.rkt")
(require (except-in "macro.rkt" make-label))
(require "compiler-stages.rkt")
(require/typed "primops.rkt"
  [ make-primop-table (-> PrimopTable)])

(require (submod "typed.rkt" binaryio))

(provide main)

(module unsafe typed/racket/no-check
  (require "globals.rkt")
  (require "debugger.rkt")
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
     [("-d")             "Print LOGN debug messages"
                         (*on-log* on-log-print)]
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

(: verbose-output (-> Syntax Void))
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

(: install-primops! (-> Void))
(define (install-primops!)
  (*primops* (make-primop-table)))

(: main (-> String Void))
(define (main filename)
  (current-output-port (current-error-port))
  (install-primops!)
  (%-install-macro-library!)
  (let ([ prog (read-file filename #:execute? #t)])
    (cond ((*test?*)    (assert-test filename prog))
          ((verbose? 1) (verbose-output prog))
          (else         (standard-output prog)))
    (when (verbose? VERBOSITY-MEDIUM)
      (display-macros))
    ))

(module* main #f
  (require/typed (submod ".." unsafe) [ file-to-compile String ])
  (main file-to-compile))
