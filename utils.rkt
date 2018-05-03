#lang typed/racket

(require typed/racket/unsafe)
(require (submod "types.rkt" common))
(require (submod "typed.rkt" binaryio))
(require (submod "typed.rkt" dict))

(provide (all-defined-out)
         (all-from-out 'struct)
         dotted-map
         list->dottable
         unsafe-cast
         )

(define WORDLIMIT (arithmetic-shift 1 256))
(define SIGNEDLIMIT (arithmetic-shift 1 255))

(: maybe->list (All (A) (-> Boolean A (Listof A))))
(define (maybe->list pred? x)
  (if pred?
      (list x)
      '()))

(: integer-bytes (-> Integer Byte))
(define (integer-bytes n)
  (cond ((< n 256)        1)
        ((< n 65536)      2)
        ((< n 16777216)   3)
        ((< n 4294967296) 4)
        ; TODO: Fill in this table to 32
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


(: tick-counter! (-> (Parameter Nonnegative-Integer) Nonnegative-Integer))
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

(: byte-or-zero (-> Bytes Nonnegative-Integer Byte))
(define (byte-or-zero bs i)
  (if (>= i (bytes-length bs))
      0
      (bytes-ref bs i)
      ))

(: bytes-or-zero (-> Bytes Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer))
(define (bytes-or-zero bs i len)
  (if (>= i (bytes-length bs))
      0
      (let ([result (bytes->integer bs #f #t i
                                    (min (+ i len)
                                         (bytes-length bs)))])
        (assert result exact-nonnegative-integer?)
        result)))

(: symbol->integer (-> Symbol Natural)) ; TODO: I think the "official" ABI uses a Keccak hash for this.
(define (symbol->integer sym)
  (let ((lst (string->list (symbol->string sym))))
    (: loop (-> (Listof Char) Natural Natural))
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

;(: assert-0..∞ (-> Integer Nonnegative-Integer))
(define-syntax-rule (assert-natural n) (cast n Natural))

(: truncate-int (-> Integer Nonnegative-Integer))
(define (truncate-int x)
  (set! x (modulo x WORDLIMIT))
  (assert-natural
   (if (>= x WORDLIMIT)
       (- x WORDLIMIT)
       (if (< x 0)
           (+ WORDLIMIT x)
           x))))


(: symbol-append (-> Symbol * Symbol))
(define (symbol-append . xs)
  (string->symbol (apply string-append (map symbol->string xs))))

(: word->integer (-> Natural Integer))
(define (word->integer w)
  (if (>= w SIGNEDLIMIT)
      (- w WORDLIMIT)
      w))

(: word->bytes (-> Natural Bytes))
(define (word->bytes x)
  (integer->bytes x 32 #f))

;; xxxxxxxxxxxx11111111 -> 11111111111111111
;; xxxxxxxxxxxx01111111 -> 00000000001111111
(: sign-extend (-> EthWord Natural EthWord))
(define (sign-extend x num-bytes)
  (define num-bits (* num-bytes 8))
  (define word-limit (arithmetic-shift 1 num-bits))
  (define signed-limit (arithmetic-shift 1 (- num-bits 1)))
  (: signed-x EthInt)
  (define signed-x (if (>= x signed-limit)
                       (- (modulo x word-limit) word-limit)
                       (modulo x signed-limit)))
  ;; (pretty-print `(DEBUG SIGN
  ;;                       (x ,x)
  ;;                       (num-bits ,num-bits)
  ;;                       (word-limit ,word-limit)
  ;;                       (signed-limit ,signed-limit)
  ;;                       (signed-x ,signed-x)))
  (truncate-int signed-x)
  )
(module* test racket
  (require rackunit)
  (require (submod ".."))
  (check-equal? (truncate-int -1) (sign-extend 255 1))
  (check-equal? (truncate-int -2) (sign-extend 65534 2))
  (check-equal? 255 (sign-extend 255 2))
  (check-equal? (truncate-int -1) (sign-extend (truncate-int -1) 1))
  )

(module syntax-parse racket
  (require syntax/parse)
  (provide datum)
  (define-syntax-rule (datum x)
    (syntax->datum (attribute x)))
  )

(module unsafe racket
  (provide (all-defined-out))
  (define (dotted-map f xs)
    (match xs
      ['() '()]
      [`(,x) `(,(f x))]
      [(cons #{a : A} (? pair? b)) (cons (f a) (dotted-map f b))]
      [(cons a b) (cons (f a) (f b))]
      ))
  (define (list->dottable xs) xs)
  (define (unsafe-cast x) x)
  )

(unsafe-require/typed 'unsafe
                      [ dotted-map (All (A B) (-> (-> A B) (DottableListof A A) (DottableListof B B)))]
                      [ list->dottable (All (A B) (-> (Listof A) (DottableListof A B)))]
                      [ unsafe-cast (All (A B) (-> A B))]
                      )
