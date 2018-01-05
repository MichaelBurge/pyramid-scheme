#lang typed/racket/no-check

(require racket/cmdline)
(require file/sha1)

(require "ast.rkt")
(require "codegen.rkt")
(require "serializer.rkt")
(require "compiler.rkt")
(require "types.rkt")
(require "io.rkt")
(require "disassembler.rkt")
(require "test.rkt")
(require "analysis.rkt")

(define *verbose?* (make-parameter #f))
(define *test?* (make-parameter #f))
; (define optimize-level (make-parameter 0))
 
(define file-to-compile
  (command-line
   #:program "pyramid"
   #:once-each
   [("-v" "--verbose") "Compile with verbose messages. Used to debug the compiler."
                       (*verbose?* #t)]
   [("-g")             "Compile with debug symbols"
                       (*use-debug-symbols?* #t)]
   [("-t")             "Run file as a test"
                       (*test?* #t)]
   [("-l" "--link")
    val
    "Generate deployable Ethereum bytecode"
    (*link?* val)]
   #:args (filename) ; expect one command-line argument: <filename>
   filename))

(: verbose-output (-> Pyramid Void))
(define (verbose-output prog)
  (let* (;(the-global-environment (setup-environment))
         (instructions           (compile-pyramid prog 'val 'next))
         (eth-instructions       (codegen (inst-seq-statements instructions)))
         (bs                     (maybe-link (serialize-with-relocations eth-instructions))))
    (begin
      (newline) (display "Abstract Instructions:") (newline)
      (display-all (inst-seq-statements instructions))
  
      ; (newline) (display "EVM Instructions:") (newline) (display-all eth-instructions)
      ; (print-symbol-table *symbol-table*)
      (print-relocations *relocation-table*)
      (print-disassembly bs))))

(: standard-output (-> Pyramid Void))
(define (standard-output prog)
  (let ([bs (third (full-compile prog))])
    (write (bytes->hex-string bs))
    (newline)))

(define (main)
  (let* ((fh (open-input-file file-to-compile))
         (prog (read fh)))
    (cond ((*test?*)    (run-test file-to-compile prog))
          ((*verbose?*) (verbose-output prog))
          (else         (standard-output prog)))
    (when (*verbose?*)
      (display-macros))
    ))
(main)
