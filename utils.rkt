#lang typed/racket

(require "typed/dict.rkt")

(provide (all-defined-out))

(: integer-bytes (-> Integer Byte))
(define (integer-bytes n)
  (cond ((< n 256)        1)
        ((< n 65536)      2)
        ((< n 16777216)   3)
        ((< n 4294967296) 4)
        (else            32)))

(: invert-hash (All (A B) (-> (Mutable-HashTable A B) (Mutable-HashTable B A))))
(define (invert-hash hash)
  (: elems (Listof (Pairof A B)))
  (define elems (hash->list hash))

  (: swap-pair (-> (Pairof A B) (Pairof B A)))
  (define (swap-pair p) (cons (cdr p) (car p)))
  (make-hash (map swap-pair elems)))

(: implies-f (All (A B) (-> (-> A Boolean) (-> A B) A (U B #f))))
(define (implies-f pred on-true x)
  (if (pred x)
      (on-true x)
      #f
      ))

(: undefined (All (A) (-> A)))
(define (undefined) (error "undefined"))

(: debug-print (-> Any * Void))
(define (debug-print . xs)
  (displayln xs))

;; (define (bool-f pred on-true on-false x) (implies (pred x) (get x)))
;; (define (const x) (λ (y) x))
  
