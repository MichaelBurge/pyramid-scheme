#lang typed/racket/no-check

(provide (all-defined-out))

(define (read-statements filename)
  (let loop ([fh (open-input-file filename) ])
    (let ([ x (read fh) ])
      (if (eof-object? x)
          null
          (cons x
                (loop fh))))))

(: read-file (-> String Pyramid))
(define (read-file filename)
  (cons 'begin
        (read-statements filename)))
