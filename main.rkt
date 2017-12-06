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

(define *verbose?* (make-parameter #f))
; (define optimize-level (make-parameter 0))
(define *link?* (make-parameter #t))
 
(define file-to-compile
  (command-line
   #:program "pyramid"
   #:once-each
   [("-v" "--verbose") "Compile with verbose messages. Used to debug the compiler."
                       (*verbose?* #t)]
   [("-g")             "Compile with debug symbols"
                       (*use-debug-symbols?* #f)]
   [("-l" "--link")
    val
    "Generate deployable Ethereum bytecode"
    (*link?* val)]
   #:args (filename) ; expect one command-line argument: <filename>
   filename))

(: maybe-link (-> bytes bytes))
(define (maybe-link bs)
  (if (*link?*)
      (wrap-loader bs)
      bs))

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
  (let* (;(the-global-environment (setup-environment))
         (instructions           (compile-pyramid prog 'val 'next))
         (eth-instructions       (codegen (inst-seq-statements instructions)))
         (bs                     (maybe-link (serialize-with-relocations eth-instructions))))
    (write (bytes->hex-string bs))
    (newline)))

(define (main)
  (let* ((fh (open-input-file file-to-compile))
         (prog (read fh)))
    (if (*verbose?*)
        (verbose-output prog)
        (standard-output prog))))

(main)
