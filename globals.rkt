#lang typed/racket

(require "utils.rkt")
(require "types.rkt")

(provide (all-defined-out))

(define VERBOSITY-NONE 0)
(define VERBOSITY-LOW 1)
(define VERBOSITY-MEDIUM 2)
(define VERBOSITY-HIGH 3)
(define VERBOSITY-ALL 4)

; All shared global variables should be moved here

(: *verbosity* (Parameterof Fixnum))
(define *verbosity* (make-parameter 0))

(: verbose? (-> Fixnum Boolean))
(define (verbose? n) (>= (*verbosity*) n))

(: *test?* (Parameterof Boolean))
(define *test?* (make-parameter #f))

(: *minimize?* (Parameterof Boolean))
(define *minimize?* (make-parameter #f))

(: *simplify?* (Parameterof Boolean))
(define *simplify?* (make-parameter #t))
; (define optimize-level (make-parameter 0))

(: *include-directory* (Parameterof String))
(define *include-directory* (make-parameter "."))

(: *test-contract* (Parameterof (U #f Address)))
(define *test-contract* (make-parameter #f))

(: *test-suite* (Parameterof test-suite))
(define *test-suite* (make-parameter (test-suite "undefined" '())))

(: *exports* (Parameterof (Listof Any)))
(define *exports* (make-parameter null)) ; Used to generate the standard ABI for the current Pyramid contract

(: *loader-size* (Parameterof Integer))
(define *loader-size* (make-parameter 0)) ; Size of the most recently generated loader

(: *byte-offset* (Parameterof UnlinkedOffset))
(define *byte-offset* (make-parameter 0)) ; Used during serialization to track output stream position

(: *abstract-offset* (Parameterof Integer))
(define *abstract-offset* (make-parameter 0)) ; Used to generate debug labels. Index of the last-generated abstract machine instruction.

(: *symbol-table* (Parameterof SymbolTable))
(define *symbol-table* (make-parameter (make-symbol-table)))

(: *relocation-table* (Parameterof RelocationTable))
(define *relocation-table* (make-parameter (make-relocation-table)))

(: *reverse-symbol-table* (Parameter ReverseSymbolTable))
(define *reverse-symbol-table* (make-parameter (make-reverse-symbol-table)))

(: *available-macros* (Parameterof Namespace))
(define *available-macros* (make-parameter (make-empty-namespace)))

(: *macro-namespace* (Parameterof Namespace))
(define *macro-namespace* (make-parameter (make-base-namespace)))

(: *patchpoints* (Parameterof (Listof patchpoint)))
(define *patchpoints* (make-parameter null))

(: *required-modules* (Parameterof (Setof Any)))
(define *required-modules* (make-parameter (set)))


; Constants
(define assumed-label-size 2) ; TODO: Number of bytes to leave behind for label relocations. This makes it difficult to write programs larger than 65536 bytes.
