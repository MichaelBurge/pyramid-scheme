#lang racket

(require (for-syntax "expander.rkt"))
(require (submod "expander.rkt" expanders))
(require (submod "types.rkt" pyramidc))
(require (submod "types.rkt" ast))
(provide read
         read-syntax
         (rename-out [pyramid-module-begin #%module-begin])
         translation-unit
         )

(module reader racket
  (require "reader.rkt")
  (require "expander.rkt")
  (require "types.rkt")
  (require "globals.rkt")
  (require racket/pretty)

  (provide read read-syntax)

  (define (read in)
    (syntax->datum (read-syntax #f in)))

  (define (read-syntax path port)
    (define stxs (read-pyramid-syntaxes path port))
    (define module-stx #`(module pyr-mod pyramid
                             (begin #,@stxs)))
    module-stx
    ;; (displayln tokens)
    ;; (displayln all-token-types)
    ;(pretty-print (parse-to-datum tokens))
    ))

(define lang-expander-options (expander-options #t))

(define-syntax (pyramid-module-begin stx)
  (syntax-case stx ()
    [(_ x) #`(#%module-begin
              (provide make-translation-unit)

              ; (: make-translation-unit (-> Boolean translation-unit))
              (define (make-translation-unit execute?)
                (translation-unit 'pyramid
                                  (quote x)
                                  #f
                                  (expand-pyramid #'x #:options lang-expander-options)
                                  '()
                                  )
                )
              )]))
