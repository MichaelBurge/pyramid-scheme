#lang typed/racket

(provide (all-defined-out))

(: unsafe-cast (All (A) (-> A A)))
(define (unsafe-cast x) x)
