#lang typed/racket/no-check

(provide (all-defined-out))

; All global variables should be moved here

(define *verbose?* (make-parameter #f))
(define *test?* (make-parameter #f))
; (define optimize-level (make-parameter 0))

(define *include-directory* (make-parameter "."))

(define *test-expected-result* (make-parameter null))
(define *exports* (make-parameter null)) ; Used to generate the standard ABI for the current Pyramid contract
(define *loader-size* (make-parameter 0)) ; Size of the most recently generated loader

(define *byte-offset* (make-parameter 0)) ; Used during serialization to track output stream position
(define *symbol-table* (make-parameter (make-hash '())))
(define *relocation-table* (make-parameter (set)))
(define *reverse-symbol-table* (make-parameter null))
(define *available-macros* (make-parameter (make-empty-namespace)))
(define *macro-namespace* (make-parameter (make-base-namespace)))
