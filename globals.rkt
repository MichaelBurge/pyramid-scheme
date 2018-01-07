#lang typed/racket/no-check

(provide (all-defined-out))

; All global variables should be moved here

(define *verbose?* (make-parameter #f))
(define *test?* (make-parameter #f))
; (define optimize-level (make-parameter 0))

(define *include-directory* (make-parameter "."))

(define *test-expected-result* (make-parameter null))
(define *exports* (make-parameter null)) ; Used to generate the standard ABI for the current Pyramid contract
