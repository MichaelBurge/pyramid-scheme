#lang typed/racket

(require typed/racket/unsafe)
(require (submod "types.rkt" common))
(require (submod "types.rkt" ast))
(require (submod "types.rkt" abstract-machine))
(require (submod "types.rkt" evm-assembly))
(require "utils.rkt")
(require "globals.rkt")

(require "typed/binaryio.rkt")

(require/typed racket/pretty
  [ pretty-print (-> Any Void)])

(unsafe-provide read-statements-port)

(provide (except-out (all-defined-out) read-statements-port))

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
     (pyr-asm (map parse-asm (cast ops PyramidQs))))
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
         (error "expand-pyramid: Unexpected form" x)))))

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

(: parse-evm (-> PyramidQ EthInstruction))
(define (parse-evm x)
  (match x
    [`(push  'shrink         ,val) (evm-push 'shrink           (cast val EthWord))]
    [`(push  ,(? byte? size) ,val) (evm-push  (cast size Byte) (cast val EthWord))]
    [`(op    (quote ,x))           (evm-op    (cast x Symbol))]
    [`(byte  ,(? exact-integer? val))       (evm-bytes (bytes (cast val Byte)))]
    [`(bytes ,(? exact-integer? size)
             ,(? exact-integer? val))
     (evm-bytes (integer->bytes val size #f))]
    [`(label (quote ,(? symbol? name))) (label-definition (cast name Symbol) 0  #f)]
    [`(label (quote ,(? symbol? name))
             ,(? exact-integer? os))    (label-definition (cast name Symbol) os #f)]
    ;; [`(,(? symbol? name) . ,(? list? args))
    ;;  (pyr-asm-cg `(,name ,@args))]
    [_ (error "parse-asm: Unknown syntax" x)]))

(: shrink-evm (-> EthInstruction PyramidQ))
(define (shrink-evm asm)
  (match asm
    [(struct evm-push ('shrink value))       `(push 'shrink ,value)]
    [(struct evm-push (size value))          `(push ,size ,value)]
    [(struct evm-op   (sym))                 `(op (quote ,sym))]
    [(struct evm-bytes (bs)) (match (bytes-length bs)
                               [ 1           `(byte ,(first (bytes->list bs)))]
                               [ n           `(bytes ,n ,(bytes->integer bs #f))])]
    ;[(struct pyr-asm-cg (exp))                   exp]
    [(struct label-definition (name 0  #f))      `(label (quote ,name))]
    [(struct label-definition (name os #f))      `(label (quote ,name) ,os)]
    [(struct label-definition (name os virtual)) `(label (quote ,name) ,os ,virtual)]
    [_ (error "shrink-asm: Unknown syntax" asm)]))

(: parse-asm (-> PyramidQ Instruction))
(define (parse-asm x)
  (match x
    [`(label (quote ,(? symbol? name))) (label-definition name 0 #f)]
    [`(label (quote ,(? symbol? name)) ,(? exact-integer? os)) (label-definition name os #f)]
    [`(label (quote ,(? symbol? name)) ,(? exact-integer? os) ,(? boolean? virtual?)) (label-definition name os virtual?)]
    [`(assign (quote ,(? register? name)) ,value) (assign name (parse-mexpr value))]
    [`(test ,condition) (test (parse-mexpr condition))]
    [`(branch ,dest) (branch (parse-mexpr dest))]
    [`(goto ,dest) (goto (parse-mexpr dest))]
    [`(save ,e) (save (parse-mexpr e))]
    [`(restore (quote ,(? register? name))) (restore name)]
    [`(perform ,e) (perform (cast (parse-mexpr e) op))]
    [`(evm . ,(? list? is)) (evm (map parse-evm is))]
    [_ (error "parse-asm: Unknown syntax" x)]
    ))

(: shrink-asm (-> Instruction PyramidQ))
(define (shrink-asm i)
  (match i
    [(struct label-definition (name 0        #f)) `(label   (quote ,name)                      )]
    [(struct label-definition (name os       #f)) `(label   (quote ,name) ,os                  )]
    [(struct label-definition (name os virtual?)) `(label   (quote ,name) ,os ,virtual?        )]
    [(struct assign           (name       value)) `(assign  (quote ,name) ,(shrink-mexpr value))]
    [(struct test             (condition       )) `(test    ,(shrink-mexpr condition)          )]
    [(struct branch           (dest            )) `(branch  ,(shrink-mexpr dest)               )]
    [(struct goto             (dest            )) `(goto    ,(shrink-mexpr dest)               )]
    [(struct save             (exp             )) `(save    ,(shrink-mexpr exp)                )]
    [(struct restore          (name            )) `(restore (quote ,name)                      )]
    [(struct perform          (op              )) `(perform ,(shrink-mexpr op)                 )]
    [(struct evm              (is              )) `(evm     ,@(map shrink-evm is)              )]
    [_ (error "shrink-abstract: Unknown syntax" i)]
    ))

(: parse-mexpr (-> PyramidQ MExpr))
(define (parse-mexpr x)
  (match x
    [`(reg (quote ,(? register? name)))        (reg name)]
    [`(const ,(? register-value? value))       (const value)]
    [`(boxed-const ,(? register-value? value)) (boxed-const value)]
    [`(op (quote ,(? symbol? name)) . ,(? list? args)) (op name (map parse-mexpr args))]
    ['stack                                    stack]
    [`(label (quote ,(? symbol? name)))        (label name)]
    [`(evm (quote ,xs) ...)                    (evm (map evm-op (cast xs (Listof Symbol))))]
    [_ (error "parse-mexpr: Unknown syntax" x)]
    ))

(: shrink-mexpr (-> MExpr PyramidQ))
(define (shrink-mexpr e)
  (match e
    [(struct reg         (name     )) `(reg (quote ,name) )]
    [(struct const       (value    )) `(const ,value      )]
    [(struct boxed-const (value    )) `(boxed-const ,value)]
    [(struct op          (name args)) `(op ,name ,@(map shrink-mexpr args))]
    [(struct %stack      (         )) 'stack               ]
    [(struct label       (name))      `(label ,name)]
    [(struct evm         (is))        `(evm ,@(map (λ (x) (list 'quote x)) (map evm-op-name (cast is (Listof evm-op)))))]
    [_ (error "shrink-mexpr: Unknown syntax" e)]
    ))
     
(: syntaxes (Setof Symbol))
(define syntaxes (apply set '(quote set! define if lambda begin defmacro push op byte label asm)))

(: register? (-> Any Boolean : #:+ RegisterName))
(define (register? x)
  (match x
    ['env      #t]
    ['proc     #t]
    ['continue #t]
    ['argl     #t]
    ['val      #t]
    [_         #f]
    ))

(: pyr-identifier? (-> Any Boolean : #:+ Symbol))
(define (pyr-identifier? x)
  (and (symbol? x)
       (not ((inst set-member? Symbol) syntaxes x))))

(: defined-macro? (-> Any Boolean : #:+ Symbol))
(define (defined-macro? name)
  (and (symbol? name)
       (namespace-contains? (*available-macros*) name)))

(: read-statements-port (-> Path Input-Port (Listof (Syntaxof PyramidQ))))
(define (read-statements-port path fh)
  (: loop (-> (Listof (Syntaxof PyramidQ))))
  (define (loop)
    (let ([ x (read-syntax path fh) ])
      ;(displayln x)
      ;(displayln (continuation-mark-set-first (current-continuation-marks) '(line)))
      ;(displayln (continuation-mark-set->list* (current-continuation-marks) '(line)))
      (if (eof-object? x)
          null
          (cons x (loop)))))
  (loop))


;; (: read-statements (-> String (Listof Any)))
;; (define (read-statements filename)
;;   (read-statements-port (open-input-file filename)))

;; (: read-file (-> String (Listof Any)))
;; (define (read-file filename)
;;   `(begin ,@(read-statements filename)))

(module unsafe racket
  (provide parse-file)
  (define (parse-file filename)
    (eval `(begin (require ,filename) program))
    )
  ;(expand-pyramid (read-file filename)))
)

(require/typed 'unsafe
  [ parse-file (-> String Pyramid) ])

(: read-file (-> String PyramidQ))
(define (read-file filename)
  (shrink-pyramid (parse-file filename)))

(: sequence->exp (-> Pyramids Pyramid))
(define (sequence->exp seq)
  (match seq
    (`() (pyr-begin '()))
    ((list x) x)
    (xs (pyr-begin xs))))

