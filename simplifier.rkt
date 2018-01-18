#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "ast.rkt")
(require "io.rkt")
(require racket/pretty)

(provide (all-defined-out))

(define (simplify prog)
  (set! prog (fixpass pass-expand-macros prog))
  ;(pretty-print prog)
  (set! prog (pass-inline-simple-definitions prog))
  (set! prog (fixpass pass-remove-unused-definitions prog))
  ;(pretty-print prog)
  (set! prog (pass-error-on-undefined-variables prog))
  ;(pretty-print prog)
  (set! prog (pass-collapse-nested-begins prog))
  ;(pretty-print prog)
  prog
  )


(define (pass-expand-macros prog)
  (set! prog (transform-ast-descendants-on prog macro?
                                           (λ (x) (begin
                                                    (install-macro-exp! x)
                                                    '(begin)))))
  (set! prog (fixpass (λ (x)
                        (transform-ast-descendants-on x macro-application? expand-macro))
                      prog))
  prog
  )
  
(define (pass-remove-unused-definitions prog)
  (let ([ unuseds (set-subtract (defined-vars prog) (used-vars prog))])
    (remove-definitions prog unuseds)))

(define (pass-error-on-undefined-variables prog)
  (let ([ undefineds (set-subtract (used-vars prog) (defined-vars prog))])
    (if (set-empty? undefineds)
        prog
        (begin
          (pretty-print prog)
          (error "Undefined variables" undefineds)))))

(define (pass-collapse-nested-begins prog)
  (transform-ast-descendants-on
   prog
   begin?
   (λ (x)
     (make-begin
      (apply append
             (map (λ (y)
                    (if (begin? y)
                        (begin-actions y)
                        (list y)))
                  (begin-actions x)))))))

(define (pass-inline-simple-definitions prog)
  (define nonsimple-definitions (mutable-set))
  (define simple-definitions (make-hash))
  (define (simple-definition? x)
    (and (definition? x)
         (not (set-member? nonsimple-definitions (definition-variable x)))
         (match (definition-value x)
           [(? self-evaluating?) #t]
           [(? variable?)        #t]
           [(? quoted?)          #t]
           [_                    #f])))
  ; Mutated variables can't be inlined.
  (set! prog (transform-ast-descendants-on
              prog assignment?
              (λ (x) (begin
                       (set-add! nonsimple-definitions (assignment-variable x))
                       x))))
  ; Remove simple definitions
  (set! prog (transform-ast-descendants-on
              prog simple-definition?
              (λ (x) (begin
                       (hash-set! simple-definitions
                                  (definition-variable x)
                                  (definition-value x))
                       '(begin)))))
  (set! prog (transform-ast-descendants-on
              prog variable?
              (λ (x) (hash-ref simple-definitions x x))))
  prog)
                       

(define (fixpass pass prog)
  (let ([ newprog (pass prog) ])
    (if (equal? prog newprog)
        prog
        (fixpass pass newprog))))

(define (remove-definitions prog vars)
  (define (transform x)
    (if (and (definition? x)
             (set-member? vars (definition-variable x)))
        (make-begin '())
        x))
  (transform-ast-descendants prog transform))

(define (defined-vars prog)
  (apply set (append (map definition-variable (all-definitions prog))
                     (apply append (map lambda-parameters (all-lambdas prog))))))

(define (used-vars prog)
  (apply set (all-variables prog)))

