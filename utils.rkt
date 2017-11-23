#lang typed/racket/no-check

(provide (all-defined-out))

(: integer-bytes (-> Integer Fixnum))
(define (integer-bytes n)
  (cond ((< n 256)        1)
        ((< n 65536)      2)
        ((< n 16777216)   3)
        ((< n 4294967296) 4)
        (else            32)))

(: invert-dict (-> (Dict Any) (Dict Any)))
(define (invert-dict dict)
  (make-hash
   (map (lambda (x) (cons (cdr x) (car x)))
        (dict->list dict))))
