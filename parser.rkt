#lang typed/racket

(require "types.rkt")
(require "utils.rkt")
(require "globals.rkt")

(require "typed/binaryio.rkt")

(require/typed racket/pretty
  [ pretty-print (-> Any Void)])

(provide (all-defined-out))

(: expand-pyramid (-> PyramidQ Pyramid))
(define (expand-pyramid x)
  (match x
    ((? boolean?) (pyr-const x))
    ((? exact-integer?) (pyr-const x))
    ((? string?) (pyr-const x))
    ((? symbol?) (pyr-variable x))
    (`(quote ,exp) (pyr-quoted (expand-pyramid exp)))
    (`(set! ,(? symbol? name) ,body)
     (pyr-assign name (expand-pyramid body)))
    (`(define ,(? symbol? name) . ,(? list? body))
     (pyr-definition name (sequence->exp (map expand-pyramid body))))
    (`(define (,(? symbol? name) . ,(? list? parameters)) . ,(? list? body))
     (pyr-definition name
                     (pyr-lambda (cast parameters (Listof VariableName))
                                 (sequence->exp (map expand-pyramid body)))))
    (`(,(or 'λ 'lambda) ,body) (pyr-lambda '() (expand-pyramid body)))
    (`(,(or 'λ 'lambda) ,parameters . ,(? list? body))
     (let ([ ex-body (sequence->exp (map expand-pyramid body))])
       (if (list? parameters)
           (pyr-lambda (cast parameters (Listof VariableName))
                       ex-body)
           (pyr-lambda (list (cast parameters Symbol))
                       ex-body))))
    (`(,(or 'λ 'lambda) . ,(? list? body))
     (pyr-lambda '() (sequence->exp (map expand-pyramid body))))
    (`(if ,pred ,cons ,alt)
     (pyr-if (expand-pyramid pred)
             (expand-pyramid cons)
             (expand-pyramid alt)))
    (`(begin . ,(? list? body))
     (sequence->exp (map expand-pyramid body)))
    ((list-rest 'asm ops)
     (pyr-asm (map parse-asm (cast ops (Listof Any)))))
    (`(defmacro ,(? pyr-identifier? name) . ,(? list? body))
     (pyr-macro-definition name '() body))
    (`(defmacro (,name . ,args) . ,body)
     (assert (pyr-identifier? name))
     (assert (list? body))
     (pyr-macro-definition name args `(begin ,@body)))
    (`(,(? defined-macro? head) . ,(? list? tail))
     (pyr-macro-application head (map expand-pyramid tail)))
    (`(,head . ,(? list? tail))
     (pyr-application (expand-pyramid head) (map expand-pyramid tail)))
    (_ (begin
         (pretty-print x)
         (error "expand-pyramid: Unexpected form")))))

(: shrink-pyramid (-> Pyramid PyramidQ))
(define (shrink-pyramid x)
  (match x
    [(struct pyr-const (v))                         v]
    [(struct pyr-variable (v))                      v]
    [(struct pyr-quoted (exp))                      `(quote ,(shrink-pyramid exp))]
    [(struct pyr-assign (name value))               `(set! ,name ,(shrink-pyramid value))]
    [(struct pyr-definition (name body))            `(define ,name ,(shrink-pyramid body))]
    [(struct pyr-lambda (vars body))                `(λ ,vars ,(shrink-pyramid body))]
    [(struct pyr-if (pred cons alt))                `(if ,(shrink-pyramid pred) ,(shrink-pyramid cons) ,(shrink-pyramid alt))]
    [(struct pyr-begin (actions))                   `(begin ,@(map shrink-pyramid actions))]
    [(struct pyr-macro-definition (name args body)) `(defmacro (,name ,@args) ,body)]
    [(struct pyr-macro-application (name args))     `(,name ,@(map shrink-pyramid args))]
    [(struct pyr-asm (ops))                         `(asm ,@(map shrink-asm ops))]
    [(struct pyr-application (op xs))               `(,(shrink-pyramid op) ,@(map shrink-pyramid xs))]
    [_ (error "shrink-pyramid: Unknown syntax" x)]
    ))

(: shrink-asm (-> pyr-asm-base PyramidQ))
(define (shrink-asm asm)
  (match asm
    [(struct pyr-asm-push ('shrink value))       `(push 'shrink ,value)]
    [(struct pyr-asm-push (size value))          `(push ,size ,value)]
    [(struct pyr-asm-op   (sym))                 `(op (quote ,sym))]
    [(struct pyr-asm-bytes (bs)) (match (bytes-length bs)
                                   [ 0           `(byte ,(first (bytes->list bs)))]
                                   [ n           `(bytes ,(bytes->integer bs #f))])]
    [(struct pyr-asm-cg (exp))                   exp]
    [(struct label-definition (name 0  #f))      `(label (quote ,name))]
    [(struct label-definition (name os #f))      `(label (quote ,name) ,os)]
    [(struct label-definition (name os virtual)) `(label (quote ,name) ,os ,virtual)]
    [_ (error "shrink-asm: Unknown syntax" asm)]))

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
    [`(push  'shrink         ,val) (pyr-asm-push 'shrink           (cast val EthWord))]
    [`(push  ,(? byte? size) ,val) (pyr-asm-push  (cast size Byte) (cast val EthWord))]
    [`(op    (quote ,x))           (pyr-asm-op    (cast x Symbol))]
    [`(byte  ,(? byte? val))       (pyr-asm-bytes (bytes (cast val Byte)))]
    [`(bytes ,(? exact-integer? size)
             ,(? exact-integer? val))
     (pyr-asm-bytes (integer->bytes val size #f))]
    [`(label (quote ,(? symbol? name))) (label-definition (cast name Symbol) 0  #f)]
    [`(label (quote ,(? symbol? name))
             ,(? exact-integer? os))    (label-definition (cast name Symbol) os #f)]
    [`(,(? symbol? name) . ,(? list? args))
     (pyr-asm-cg `(,name ,@args))]
    [_ (error "parse-asm: Unknown syntax" x)]))

(: read-statements (-> String (Listof Any)))
(define (read-statements filename)
  (let loop ([fh (open-input-file filename) ])
    (let ([ x (read fh) ])
      ;(displayln x)
      ;(displayln (continuation-mark-set-first (current-continuation-marks) '(line)))
      ;(displayln (continuation-mark-set->list* (current-continuation-marks) '(line)))
      (if (eof-object? x)
          null
          (cons x
                (loop fh))))))

(: read-file (-> String (Listof Any)))
(define (read-file filename)
  `(begin ,@(read-statements filename)))

(: parse-file (-> String Pyramid))
(define (parse-file filename)
  (expand-pyramid (read-file filename)))

(: sequence->exp (-> Pyramids Pyramid))
(define (sequence->exp seq)
  (match seq
    (`() (pyr-begin '()))
    ((list x) x)
    (xs (pyr-begin xs))))
