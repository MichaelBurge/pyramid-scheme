#lang typed/racket

(require "typed/binaryio.rkt")
(require "typed/dict.rkt")

(provide (all-defined-out)
         (all-from-out 'struct))

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

(: symbol->integer (-> Symbol Integer)) ; TODO: I think the "official" ABI uses a Keccak hash for this.
(define (symbol->integer sym)
  (let ((lst (string->list (symbol->string sym))))
    (: loop (-> (Listof Char) Integer Integer))
    (define (loop lst i)
      (if (null? lst)
          i
          (loop (cdr lst)
                (+ (char->integer (car lst))
                   (* 256 i)))))
    (loop lst 0)))

(: integer->string (-> Integer String))
(define (integer->string n)
  (define question-mark 77)
  (: integer->char-list (-> Integer (Listof Char)))
  (define (integer->char-list n)
    (if (equal? n 0)
        null
        (let*-values ([ (q r) (quotient/remainder n 256) ])
          (cons (integer->char (if (>= r 128) question-mark r))
                (integer->char-list q)))))
  (list->string
   (for/list ([ c (reverse (integer->char-list n))]
              #:when (char-graphic? c))
     c)))

(: map-parameter (All (A) (-> (Parameterof A) (-> A A) Void)))
(define (map-parameter p f) (p (f (p))))

(: floori (-> Real Integer))
(define (floori x) (cast (floor x) Integer))

(module struct racket
  (provide destruct)

  (require (for-syntax racket/syntax))
  (require (for-syntax racket/list))
  (require (for-syntax racket/struct-info))

  (define-syntax (destruct stx)
    (syntax-case stx ()
      [(_ ty id)
       (let* ([ si             (extract-struct-info (syntax-local-value #'ty))]
              [ accessors      (reverse (fourth si ))]
              [ accessor->name (λ (acc) (with-syntax ([ acc acc ])
                                          (format-id stx "~a~a" #'id (strip-prefix #'ty #'acc))))]
              [ names          (map accessor->name accessors)]
              [ make-def       (λ (name acc)
                                 (with-syntax ([ acc (datum->syntax stx acc)]
                                               [ name name ])
                                   #`(define name (acc id))))]
              [ defs           (map make-def names accessors)])
         #`(begin #,@defs))]))

  (begin-for-syntax
    (define (strip-prefix prefix name)
      (string->symbol
       (substring (symbol->string (syntax->datum name))
                  (string-length (symbol->string (syntax->datum prefix))))))))


(require 'struct)
