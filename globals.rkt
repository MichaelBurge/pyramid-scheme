#lang typed/racket/no-check

(require racket/undefined)

(provide (all-defined-out))

(define VERBOSITY-NONE 0)
(define VERBOSITY-LOW 1)
(define VERBOSITY-MEDIUM 2)
(define VERBOSITY-HIGH 3)
(define VERBOSITY-ALL 4)

; All shared global variables should be moved here

(define *verbosity* (make-parameter 0))
(define (verbose? n) (>= (*verbosity*) n))
(define *test?* (make-parameter #f))
(define *minimize?* (make-parameter #f))
(define *simplify?* (make-parameter #t))
; (define optimize-level (make-parameter 0))

(define *include-directory* (make-parameter "."))

(define *test-contract* (make-parameter null))
(define *test-suite* (make-parameter undefined))


(define *exports* (make-parameter null)) ; Used to generate the standard ABI for the current Pyramid contract
(define *loader-size* (make-parameter 0)) ; Size of the most recently generated loader

(define *byte-offset* (make-parameter 0)) ; Used during serialization to track output stream position
(define *abstract-offset* (make-parameter 0)) ; Used to generate debug labels. Index of the last-generated abstract machine instruction.

(define *symbol-table* (make-parameter (make-hash '())))
(define *relocation-table* (make-parameter (set)))
(define *reverse-symbol-table* (make-parameter null))
(define *available-macros* (make-parameter (make-empty-namespace)))
(define *macro-namespace* (make-parameter (make-base-namespace)))

(define *required-modules* (make-parameter (set)))

