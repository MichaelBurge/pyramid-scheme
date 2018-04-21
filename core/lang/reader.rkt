#lang racket

; The pyramid/core language is identical to the pyramid language except that literals are unboxed by default.

(require pyramid/globals)
(require pyramid/reader)
(require (submod pyramid/types ast))
(require racket/pretty)

(provide read read-syntax)
(define lang-expander-options (expander-options #f))

(define (read in)
  (syntax->datum (read-syntax #f in)))

(define (read-syntax path port)
  (parameterize ([ *expander-options* lang-expander-options])
    (define stxs (read-pyramid-syntaxes path port))
    (define module-stx #`(module pyr-mod pyramid/core
                           (begin #,@stxs)))
    ; (pretty-print module-stx)
    module-stx))
