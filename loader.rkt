#lang typed/racket

(require (submod "types.rkt" ast))
(require (submod "types.rkt" pyramidc))
(require (submod "expander.rkt" typed))

(provide (all-defined-out))

(module unsafe racket
  (provide parse-translation-unit
           vector->register-value)
  (require (submod "types.rkt" pyramidc))
  (require "globals.rkt")
  (require "utils.rkt")

  (define (parse-translation-unit filename execute?)
    ;(define make-translation-unit (dynamic-rerequire filename 'make-translation-unit))
    (define make-translation-unit (dynamic-require filename 'make-translation-unit))
    ;(dynamic-require filename 0)
    ;(define make-translation-unit
    ;(namespace-require filename)
    ;(define make-translation-unit (eval 'make-translation-unit))
    ;(eval `(module pyramid-loader racket/load (begin (require ,filename) program)))
    (define tu (make-translation-unit execute?))
    (destruct translation-unit tu)
    (when (verbose? VERBOSITY-MEDIUM)
      (pretty-print `(PARSE-TREE ,tu-source-code)))
    (when (verbose? VERBOSITY-LOW)
      (pretty-print `(SYNTAX-TREE ,tu-abstract-syntax)))
    (when (verbose? VERBOSITY-MEDIUM)
      (pretty-print `(DEPENDENCIES ,tu-dependencies)))
    tu
    )

  (define (vector->register-value x) x)
  )

(require/typed 'unsafe
  [ parse-translation-unit (-> String Boolean translation-unit)]
  [ vector->register-value (-> VectorTop RegisterValue)])


(: flatten-translation-units (-> translation-unit Pyramid))
(define (flatten-translation-units tu)
  (: tu-dependencies translation-units)
  (define tu-dependencies (translation-unit-dependencies tu))
  (pyr-begin (append (map flatten-translation-units tu-dependencies)
                     (list (translation-unit-pyramid-ast tu))))
  )

(: parse-file (-> String Boolean Pyramid))
(define (parse-file filename execute?)
  (define tu (parse-translation-unit filename execute?))
  (flatten-translation-units tu)
  )

(: read-file (-> String #:execute? Boolean PyramidQ))
(define (read-file filename #:execute? execute?)
  (define ast (parse-file filename execute?))
  (shrink-pyramid ast))
