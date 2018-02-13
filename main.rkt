#lang racket

(require (for-syntax "parser.rkt"))
(require "parser.rkt")
(provide read
         read-syntax
         ;
         define
         provide
         expand-pyramid
         quote
         #%module-begin
         #%app)

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
                            (provide program)
                            (define program (expand-pyramid (quote (begin ,@quoted))))))
    (define stx (datum->syntax #f module-datum))
    stx
    ;; (displayln tokens)
    ;; (displayln all-token-types)
    ;(pretty-print (parse-to-datum tokens))
    ))

(define-syntax (pyramid-module-begin stx)
  (syntax-case stx ()
    ;[(_ x) #`(#%module-begin (quote #,(expand-c #'x)))]))
    [(_ x) #`(#%module-begin #,(expand-pyramid #'x))]))
