#lang typed/racket/no-check

(require binaryio/integer)

(provide (all-defined-out))

; Example: $ echo -n 'baz(uint32,bool)' | keccak-256sum
(define (keccak-256 bs)
  (with-input-from-bytes bs
    (lambda ()
      (with-output-to-bytes
        (lambda ()
          (system "keccak-256sum"))))))

(define (keccak-256-word bs)
  (bytes->integer bs #f #t))
