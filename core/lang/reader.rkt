#lang racket

; The pyramid/core language is identical to the pyramid language except that literals are unboxed by default.

(require pyramid/parser)
(require pyramid/globals)

(provide read read-syntax)

(define (read in)
  (syntax->datum (read-syntax #f in)))

(define (read-syntax path port)
  (parameterize ([ *parser-literals-boxed?* #f ])
    (define quoted (map syntax->datum (read-statements-port path port)))
    (define module-datum `(module pyr-mod pyramid
                            (begin ,@quoted)))
    (define stx (datum->syntax #f module-datum))
    stx))
