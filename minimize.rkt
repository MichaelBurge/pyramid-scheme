#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "ast.rkt")
(require "utils.rkt")

(require racket/match)
(require racket/list)

(provide (all-defined-out))

(define (minimize pred? e)
  ; (debug-print e)
  (match (memf pred? (reductions e))
    [#f e]
    [e2 (minimize pred? (car e2)) ]))

; (: reductions (-> Pyramid (Listof Pyramid)))
(define (reductions e)
  (cond ((self-evaluating?   e) '())
        ((quoted?            e) '())
        ((asm?               e) (reductions-asm e))
        ((macro?             e) '())
        ((variable?          e) (reductions-variable e))
        ((assignment?        e) (reductions-assignment e))
        ((definition?        e) (reductions-definition e))
        ((if?                e) (reductions-if e))
        ((lambda?            e) (reductions-lambda e))
        ((begin?             e) (reductions-begin e))
        ((cond?              e) (reductions-cond e))
        ((macro-application? e) (reductions-macro-application e))
        ((application?       e) (reductions-application e))
        (else (error "reductions: Unknown expression type" e))
        ))

(define (reductions-asm e)
  (let ([ is (asm-insts e) ]
        [ mk-reduction (λ (is2) (cons 'asm is2))]
        )
    (map mk-reduction (reductions-list is))
    ))
(define (reductions-variable e) (list `(quote ,e)))
(define (reductions-assignment e)
  (let ([ vs (reductions (assignment-value e)) ]
        [ mk-reduction (λ (v) (list 'set! (assignment-variable e) v)) ])
    (map mk-reduction vs)))
    
(define (reductions-definition e)
  (let ([ vs (reductions (definition-value e)) ]
        [ mk-reduction (λ (v) (list 'define (definition-variable e) v)) ])
    (map mk-reduction vs)))

(define (reductions-if e)
  (let* ([ preds (reductions (if-predicate e)) ]
         [ on-true (if-consequent e) ]
         [ on-false (if-alternative e) ]
         [ mk-pred-reduction (λ (p) (list 'if p on-true on-false)) ])
    (cons on-true
          (cons on-false
                (map mk-pred-reduction preds)))))

(define (reductions-lambda e)
  (append (list 0)
          (map (λ (ps) (make-lambda ps (lambda-body e))) (reductions-list (lambda-parameters e)))
          (map (λ (body) (make-lambda (lambda-parameters e) body)) (reductions-sequence (lambda-body e)))))

(define (reductions-elements lst) (point-expansions lst reductions))

(define (point-expansions lst f)
  (define (helper pre post)
    (if (null? post)
        '()
        (let* ([ x (car post) ]
               [ xs (cdr post) ]
               [ seq (f x) ])
          (append (if seq
                      (for/list ([i seq])
                        (append pre (list i) xs))
                      '())
                  (helper (append pre (list x)) xs)))))
  (helper '() lst))

(define (reductions-sequence xs)
  (let ([ removes (reductions-list xs) ]
        [ reds (reductions-elements xs) ]
        [ expansions (point-expansions xs
                                       (λ (x) (if (begin? x)
                                                  (begin-actions x)
                                                  #f))) ]
        )
    (append removes reds expansions)))
  

(define (reductions-begin e)
  (let ([ mk-reduction (λ (xs) (assert-pyramid (cons 'begin xs))) ])
    (map mk-reduction (reductions-sequence (begin-actions e)))))

(define (reductions-cond e)
  (let ([ clausess (reductions-list (cond-clauses e)) ]
        [ mk-reduction (λ (clauses) (cons 'cond clauses))])
    (map mk-reduction clausess)))

(define (reductions-macro-application e) (list (expand-macro e)))
(define (reductions-application e)
  (append (list 0)
          (operands e)))

(define (reductions-list xs)
  (let ([ mk-reduction
          (λ (i)
            (let ([ hd (take xs i) ]
                  [ tl (drop xs i) ]
                  )
              (append hd (cdr tl)))) ])
    (for/list ([i (in-range (length xs)) ])
      (mk-reduction i))))

