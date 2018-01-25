#lang typed/racket

(require "types.rkt")
(require "globals.rkt")

(require "typed/binaryio.rkt")
(require "typed/dict.rkt")

(provide (all-defined-out))

(: display-abstract-instruction (-> Instruction Void))
(define (display-abstract-instruction i)
  (display `(,(*abstract-offset*) ,i)))

(: display-abstract-instructions (-> Instructions Void))
(define (display-abstract-instructions is)
  (parameterize ([ *abstract-offset* 0])
    (for ([ i is ])
      (*abstract-offset* (+ 1 (*abstract-offset*)))
      (display-abstract-instruction i)
      (newline))))

(: display-all (All (A) (-> (Listof A) Void)))
(define (display-all xs)
  (if (null? xs)
      (void)
      (begin
        (display (car xs))
        (newline)
        (display-all (cdr xs)))))

(: get-collection-directory (-> Symbol String))
(define (get-collection-directory collection) (symbol->string collection))

(define-syntax-rule (verbose-print verbosity xs ...)
  (when (verbose? verbosity) (debug-print xs ...)))

(: print-symbol-table (-> SymbolTable Void))
(define (print-symbol-table symbols)
  (: show (-> Symbol Integer Void))
  (define (show sym os)
    (display sym)
    (write-char #\tab)
    (display (integer->hex os))
    (newline))
  (: symbols-list (Listof (Pairof Symbol Integer)))
  (define symbols-list ((inst sort (Pairof Symbol Integer) Integer) (dict->list symbols) < #:key cdr))
  (for ([ symbol symbols-list ])
    (show (car symbol) (cdr symbol)))
  (newline))

(: integer->hex (-> Integer String))
(define (integer->hex n)
  (bytes->hex-string (integer->bytes n assumed-label-size #f)))

(: print-relocations (-> RelocationTable Void))
(define (print-relocations relocs)
  (display "Relocations:") (newline)
  (for ([ reloc (sort-relocations relocs) ])
    (display `(,(integer->hex (relocation-pos reloc))
               ,(relocation-symbol reloc)))
    (newline)))

(: sort-relocations (-> RelocationTable (Listof relocation)))
(define (sort-relocations relocs)
  (: sort-key (-> relocation Integer))
  (define (sort-key k) (relocation-pos k))
  (: sort-lst (-> (Listof relocation) (Listof relocation)))
  (define (sort-lst lst) ((inst sort relocation Integer) lst < #:key sort-key))
  (sort-lst (set->list relocs)))
