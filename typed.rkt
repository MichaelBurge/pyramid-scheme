#lang typed/racket

(module binaryio typed/racket
  (provide bytes->hex-string
           hex-string->bytes
           nonnegative->bytes
           integer->bytes
           bytes->integer
           bytes->nonnegative
           )

  (require/typed file/sha1
    [ bytes->hex-string (-> Bytes String)]
    [ hex-string->bytes (-> String Bytes)]
    )

  (require/typed binaryio/integer
    [ integer->bytes (case-> (-> Integer Integer Boolean Bytes) ; n size signed?
                             (-> Integer Integer Boolean Boolean Bytes))] ; n size signed? big-endian?
    [ bytes->integer (case-> (-> Bytes Boolean Integer)
                             (-> Bytes Boolean Boolean Integer)
                             (-> Bytes Boolean Boolean Integer Integer)
                             (-> Bytes Boolean Boolean Integer Integer Integer))] ; bs signed? big-endian? start end
    )
  (: nonnegative->bytes (-> Nonnegative-Integer Positive-Integer Bytes))
  (define (nonnegative->bytes n size)
    (integer->bytes n size #f))

  (: bytes->nonnegative (-> Bytes Nonnegative-Integer))
  (define (bytes->nonnegative bs)
    (define ret (bytes->integer bs #f #t))
    (assert ret exact-nonnegative-integer?)
    ret
    )
  )

(module dict typed/racket
  (provide dict->list
           dict-ref
           dict-set!)
  (require/typed racket/dict
    [ dict->list (All (A B) (-> (HashTable A B) (Listof (Pairof A B))))]
    [ dict-ref (All (A B) (case-> (-> (HashTable A B) A B) (-> (HashTable A B) A B B)))]
    [ dict-set! (All (A B) (-> (HashTable A B) A B Void))]
    )
  )

(module data/interval-map typed/racket
  (provide make-interval-map
           interval-map-set!
           interval-map-ref
           interval-map-ref/bounds
           IntervalMap)
  (require typed/racket/unsafe)

  (define-type (IntervalMap A) Any)

  (unsafe-require/typed data/interval-map
    [ make-interval-map       (All (A) (-> (IntervalMap A)))]
    [ interval-map-set!       (All (A) (-> (IntervalMap A) Integer Integer A Void))]
    [ interval-map-ref        (All (A) (-> (IntervalMap A) Integer A))]
    [ interval-map-ref/bounds (All (A) (-> (IntervalMap A) Integer (Values Integer Integer A)))]
    ))

(module json typed/racket
  (provide jsexpr->string)

  (require/typed json
    [ jsexpr->string (-> Any String)]
    ))
