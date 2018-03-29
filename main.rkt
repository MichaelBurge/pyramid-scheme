#lang racket

(require (for-syntax "parser.rkt"))
(require "parser.rkt")
(require (submod "types.rkt" pyramidc))
(provide read
         read-syntax
         (rename-out [pyramid-module-begin #%module-begin])
         translation-unit
         )

(module reader racket
  (require "parser.rkt")
  (require "types.rkt")
  (require racket/pretty)

  (provide read read-syntax)

  (define (read in)
    (syntax->datum (read-syntax #f in)))

  (define (read-syntax path port)
    (define quoted (map syntax->datum (read-statements-port path port)))
    (define module-datum `(module pyr-mod pyramid
                            (begin ,@quoted)))
    (define stx (datum->syntax #f module-datum))
    stx
    ;; (displayln tokens)
    ;; (displayln all-token-types)
    ;(pretty-print (parse-to-datum tokens))
    ))

(define-syntax (pyramid-module-begin stx)
  (syntax-case stx ()
    [(_ x) #`(#%module-begin
              (provide make-translation-unit)

              (define (make-translation-unit execute?)
                (translation-unit 'pyramid
                                  (quote x)
                                  #f
                                  (expand-pyramid (syntax->datum #'x))
                                  '()
                                  )
                )
              )]))
