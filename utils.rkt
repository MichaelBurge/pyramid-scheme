#lang typed/racket

(require "typed/binaryio.rkt")
(require "typed/dict.rkt")

(provide (all-defined-out))

(: maybe->list (All (A) (-> Boolean A (Listof A))))
(define (maybe->list pred? x) (if pred? (list x) '()))

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


(: tick-counter! (-> (Parameter Integer) Integer))
(define (tick-counter! x)
  (let ([ val (x) ])
    (x (+ 1 val))
    val))

(: listof (All (A) (-> A * (Listof A))))
(define (listof . xs) xs)

(: namespace-contains? (-> Namespace Symbol Boolean))
(define (namespace-contains? namespace name)
  (if (namespace-variable-value name #f (λ () #f) namespace)
      #t
      #f))

(: bytes-or-zero (-> Bytes Integer Integer Integer))
(define (bytes-or-zero bs i len)
  (if (>= i (bytes-length bs))
      0
      (bytes->integer bs #f #t i
                      (min (+ i len)
                           (bytes-length bs)))))
  
;; (if (>= i (bytes-length bs))
  ;;     0
  ;;     (bytes->integer (subbytes(bytes-ref bs i)))

;; (define (bool-f pred on-true on-false x) (implies (pred x) (get x)))
;; (define (const x) (λ (y) x))
  
