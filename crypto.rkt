#lang typed/racket

(require "typed/binaryio.rkt")

(provide (all-defined-out))

; Example: $ echo -n 'baz(uint32,bool)' | keccak-256sum
(: keccak-256 (-> Bytes Bytes))
(define (keccak-256 bs)
  (with-input-from-bytes bs
    (lambda ()
      (with-output-to-bytes
        (lambda ()
          (system "keccak-256sum"))))))

(: keccak-256-word (-> Bytes Integer))
(define (keccak-256-word bs)
  (bytes->integer bs #f #t))
