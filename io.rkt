#lang typed/racket

(require (submod "types.rkt" ast))
(require (submod "types.rkt" abstract-machine))
(require (submod "types.rkt" evm-assembly))
(require (submod "types.rkt" simulator))
(require "globals.rkt")
(require "expander.rkt")
(require "wallet.rkt")

(require (submod "typed.rkt" binaryio))
(require (submod "typed.rkt" dict))

(provide (all-defined-out)
         (all-from-out 'macros))

(: display-abstract-instruction (-> Instruction Void))
(define (display-abstract-instruction i)
  (match i
    [(struct pyr-asm _) (print `(,(*abstract-offset*) ,(format-ast i)))]
    [_                  (print `(,(*abstract-offset*) ,(asm->datum i)))]))


(: display-abstract-instructions (-> (U inst-seq Instructions) Void))
(define (display-abstract-instructions is)
  (parameterize ([ *abstract-offset* 0])
    (define is-lst : Instructions (if (inst-seq? is) (inst-seq-statements is) is))
    (for ([ i is-lst ])
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

(: sorted-symbol-table (-> SymbolTable (Listof (Pairof Symbol Integer))))
(define (sorted-symbol-table symbols)
  ((inst sort (Pairof Symbol Integer) Integer) (hash->list symbols) < #:key cdr))

(: print-symbol-table (-> SymbolTable Void))
(define (print-symbol-table symbols)
  (: show (-> Symbol Integer Void))
  (define (show sym os)
    (display sym)
    (write-char #\tab)
    (display (integer->hex os))
    (newline))
  (for ([ symbol (sorted-symbol-table symbols) ])
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

(: print-ast (-> Pyramid Void))
(define (print-ast ast)
  (pretty-print (syntax->datum (shrink-pyramid ast))))

(: format-ast (-> Pyramid String))
(define (format-ast ast)
  (pretty-format (shrink-pyramid ast)))

(: print-account (-> Address vm-account Void))
(define (print-account addr acc)
  (match acc
    [(struct vm-account (nonce balance root hash))
     (match (find-addr-name addr)
       [ #f   (displayln `(,addr ,nonce ,balance ,root))]
       [ name (displayln `(,name ,nonce ,balance ,root))]
       )]))

(: print-account-balances-sim (-> simulator Void))
(define (print-account-balances-sim sim)
  (displayln "Accounts")
  (: accs (Listof (Pairof Address vm-account)))
  (define accs (hash->list (simulator-accounts sim)))
  (for ([ addr-acc : (Pairof Address vm-account) accs ])
    (match addr-acc
      [ (cons addr acc) (print-account addr acc)])))


(: print-account-balances (-> vm-exec Void))
(define (print-account-balances vm)
  (print-account-balances-sim (vm-exec-sim vm)))

(define (display-macros)
  (display `(,"Mapped symbols:" ,(namespace-mapped-symbols (*available-macros*)))))

(module macros racket
  (require "globals.rkt")
  (provide (all-defined-out))
  (define-syntax-rule (verbose-section title level body)
    (when (verbose? level)
      (displayln title)
      body
      (newline)
      )))

(require 'macros)
