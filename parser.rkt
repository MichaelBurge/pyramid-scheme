#lang typed/racket

(require "types.rkt")
(require "utils.rkt")
(require "globals.rkt")

(provide (all-defined-out))

(: expand-pyramid (-> Any Pyramid))
(define (expand-pyramid x)
  (match x
    ((? number?) (pyr-const x))
    ((? string?) (pyr-const x))
    ((? symbol?) (pyr-variable x))
    (`(quote ,exp) (pyr-quoted (expand-pyramid exp)))
    ((list 'set! `(quote ,(? symbol? name)) body)
     (pyr-assign name (expand-pyramid body)))
    (`(define ,(? symbol? name) . ,(? list? body))
     (pyr-definition name (pyr-begin (map expand-pyramid body))))
    (`(define `(,(? symbol? name) . ,(? list? parameters)) . ,(? list? body))
     (pyr-definition name
                     (pyr-lambda (cast parameters (Listof VariableName))
                                 (pyr-begin (map expand-pyramid body)))))
    (`(,(or 'λ 'lambda) ,parameters . ,(? list? body))
     (pyr-lambda (cast parameters (Listof VariableName))
                 (pyr-begin (map expand-pyramid body))))
    (`(,(or 'λ 'lambda) . ,(? list? body))
     (pyr-lambda '() (pyr-begin (map expand-pyramid body))))
    (`(if ,pred ,cons ,alt)
     (pyr-if (expand-pyramid pred)
             (expand-pyramid cons)
             (expand-pyramid alt)))
    (`(begin . ,(? list? body))
     (pyr-begin (map expand-pyramid body)))
    ((list-rest 'asm ops)
     (pyr-asm (map parse-asm (cast ops (Listof Any)))))
    (`(defmacro ,(? pyr-identifier? name) . ,(? list? body))
     (pyr-macro-definition name '() body))
    (`(defmacro `(,(? pyr-identifier? name) . ,params) . ,(? list? body))
     (pyr-macro-definition name (cast params (Listof Symbol)) body))
    (`(,(? defined-macro? head) . ,tail)
     (assert tail list?)
     (pyr-macro-application head (map expand-pyramid tail)))
    (`(,(? pyr-identifier? head) . ,(? list? tail))
     (assert tail list?)
     (pyr-application (expand-pyramid head) (map expand-pyramid tail)))
    (_ (error "expand-pyramid: Unexpected form" x))))

(: shrink-pyramid (-> Pyramid Any))
(define (shrink-pyramid x)
  (match x
    [(struct pyr-const (v))                         v]
    [(struct pyr-variable (v))                      v]
    [(struct pyr-quoted (exp))                      `(quote ,exp)]
    [(struct pyr-assign (name value))               `(set! ,name ,(shrink-pyramid value))]
    [(struct pyr-definition (name body))            `(define ,name ,(shrink-pyramid body))]
    [(struct pyr-lambda (vars body))                `(λ ,vars ,(shrink-pyramid body))]
    [(struct pyr-if (pred cons alt))                `(if ,pred ,cons ,alt)]
    [(struct pyr-begin (actions))                   `(begin ,@(map shrink-pyramid actions))]
    [(struct pyr-macro-definition (name args body)) `(defmacro ,name ,args ,@body)]
    [(struct pyr-macro-application (name args))     `(,name ,@args)]
    [(struct pyr-asm (ops))                         `(asm ,@ops)]
    [(struct pyr-application (op xs))               `(,op ,@xs)]
    [_ (error "transform-ast-children: Unknown syntax" x)]
    ))

(: syntaxes (Setof Symbol))
(define syntaxes (apply set '(quote set! define if lambda begin defmacro push op byte label asm)))

(: pyr-identifier? (-> Any Boolean : #:+ Symbol))
(define (pyr-identifier? x)
  (and (symbol? x)
       (not ((inst set-member? Symbol) syntaxes x))))

(: defined-macro? (-> Any Boolean : #:+ Symbol))
(define (defined-macro? name)
  (and (symbol? name)
       (namespace-contains? (*available-macros*) name)))

(: parse-asm (-> Any pyr-asm-base))
(define (parse-asm x)
  (match x
    [(list 'push 'shrink val)     (pyr-asm-push 'shrink           (cast val EthWord))]
    [(list 'push size val)        (pyr-asm-push  (cast size Byte) (cast val EthWord))]
    [(list 'op   (list 'quote x)) (pyr-asm-op    (cast x Symbol))]
    [(list 'byte val)             (pyr-asm-byte  (cast val Byte))]
    [(list 'label name)           (pyr-asm-label (cast name Symbol))]
    [_ (error "parse-asm: Unknown syntax" x)]))

(: read-statements (-> String Pyramids))
(define (read-statements filename)
  (let loop ([fh (open-input-file filename) ])
    (let ([ x (read fh) ])
      ;(displayln x)
      ;(displayln (continuation-mark-set-first (current-continuation-marks) '(line)))
      ;(displayln (continuation-mark-set->list* (current-continuation-marks) '(line)))
      (if (eof-object? x)
          null
          (cons (expand-pyramid x)
                (loop fh))))))

(: read-file (-> String Pyramid))
(define (read-file filename)
  (pyr-begin (read-statements filename)))
