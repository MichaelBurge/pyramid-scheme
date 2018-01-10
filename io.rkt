#lang typed/racket

(require "types.rkt")

(provide (all-defined-out))

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
