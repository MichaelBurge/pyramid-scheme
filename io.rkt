#lang typed/racket/no-check

(require "types.rkt")
(require "globals.rkt")

(provide (all-defined-out))

(: display-abstract-instruction (-> Instruction Void))
(define (display-abstract-instruction i)
  (display `(,(*abstract-offset*) ,i)))

(: display-abstract-instructions (-> Instructions Void))
(define (display-abstract-instructions is)
  (parameterize ([ *abstract-offset* 0])
    (for ([ i (inst-seq-statements is) ])
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

(define (debug-print . xs)
  (apply display xs)
  (newline))

(define-syntax-rule (verbose-print verbosity xs ...)
  (when (verbose? verbosity) (debug-print xs ...)))
