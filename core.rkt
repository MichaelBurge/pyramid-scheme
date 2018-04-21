#lang racket

(require (submod pyramid/types pyramidc))
(require (submod pyramid/types ast))
(require pyramid/expander)

(provide (rename-out [pyramid-core-module-begin #%module-begin])
         translation-unit
         expand-pyramid
         )

(define lang-expander-options (expander-options #f))

(define-syntax (pyramid-core-module-begin stx)
  (syntax-case stx ()
    [(_ x) #`(#%module-begin
              (provide make-translation-unit)

              (define (make-translation-unit execute?)
                (translation-unit 'pyramid
                                  (quote x)
                                  #f
                                  (expand-pyramid (syntax->datum #'x) #:options lang-expander-options)
                                  '()
                                  )
                )
              )]))
