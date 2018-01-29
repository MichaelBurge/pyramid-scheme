#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "globals.rkt")
(require "parser.rkt")
(require "utils.rkt")

(require binaryio/integer)
(require typed/racket/unsafe)

(unsafe-provide (all-defined-out))

;(provide (all-defined-out))

; Typed Racket has trouble applying arbitrary procedures to an argument list.
; So we put those operations in this module.

(: lookup-macro (-> Symbol PyrMacro))
(define (lookup-macro name)
  (namespace-variable-value name #t #f (*available-macros*)))

(: expand-macro (-> pyr-macro-application Pyramid))
(define (expand-macro exp)
  (let* ((name (pyr-macro-application-name exp))
         (macro (lookup-macro name))
         (result (parameterize ([ current-namespace (*macro-namespace*) ])
                   (let ([ args (map shrink-pyramid (pyr-macro-application-operands exp))])
                     (apply macro args))))
         )
    (expand-pyramid result))) ; Reparse it to allow macros to return quoted forms.

(: install-macro! (-> Symbol PyrMacro Void))
(define (install-macro! name func)
  (namespace-set-variable-value! name func #t (*available-macros*)))

(: install-macro-exp! (-> pyr-macro-definition Void))
(define (install-macro-exp! exp)
  (let* ([ name (pyr-macro-definition-name exp)]
         [ args (pyr-macro-definition-parameters exp)]
         [ body (pyr-macro-definition-body exp)]
         [ macro-exp `(lambda (,@args) ,body)]
         [ macro (begin
                   ;(debug-print 'install-macro-exp name macro-exp)
                   (eval macro-exp (*macro-namespace*)))]
         )
    (install-macro! name macro)
    ))
