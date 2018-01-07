#lang typed/racket/no-check

(provide (all-defined-out))

; All global variables should be moved here

(define *verbose?* (make-parameter #f))
(define *test?* (make-parameter #f))
; (define optimize-level (make-parameter 0))
