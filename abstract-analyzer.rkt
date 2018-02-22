#lang racket

(provide #%datum #%app #%module-begin #%top-interaction quote
         verify-instructions
         (all-defined-out))

; env is a linked list of frames. A frame is a hashtable of names => values
; proc, continue are labels
; argl is a list of values
; val is a value
(struct machine (pc env proc continue argl val stack halted?) #:transparent #:mutable)

(struct instruction                  ()             #:transparent)
(struct label-definition instruction (name offset virtual?) #:transparent)
(struct assign           instruction (reg-name value) #:transparent)
(struct test             instruction (condition     ) #:transparent)
(struct branch           instruction (dest          ) #:transparent)
(struct goto             instruction (dest          ) #:transparent)
(struct save             instruction (reg-name      ) #:transparent)
(struct restore          instruction (reg-name      ) #:transparent)
(struct perform          instruction (action        ) #:transparent)
(struct asm              instruction (insts         ) #:transparent)

(struct mexpr             (         ) #:transparent)
(struct reg         mexpr (name     ) #:transparent)
(struct const       mexpr (value    ) #:transparent)
(struct boxed-const mexpr (value    ) #:transparent)
(struct op          mexpr (name args) #:transparent)

(struct value                       (            ) #:transparent)
(struct v-unboxed             value (value)        #:transparent)
(struct v-fixnum              value (value       ) #:transparent)
(struct v-symbol              value (value       ) #:transparent)
(struct v-compiled-procedure  value (label    env) #:transparent)
(struct v-primitive-procedure value (label       ) #:transparent)
(struct v-pair                value (left   right) #:transparent)
(struct v-vector              value (elems       ) #:transparent)
(struct v-nil                 value (            ) #:transparent)
(struct v-continuation        value (continue env) #:transparent)

(struct context     (state entry) #:transparent)

(define *m* (machine 0 null #f #f null #f null #f))
(define *symbol-table* (make-hash))
(define MAX-ITERATIONS 1000000)

(define (verify-instructions . is)
  (build-symbol-table is)
  (set! is (list->vector is))
  (let loop ([n 0])
    (when (>= n MAX-ITERATIONS)
      (error "verify-instructions: Didn't stop after iterations" n))
    (cond ((machine-halted? *m*) (machine-val *m*))
          ((>= n (vector-length is)) (machine-val *m*))
          (else (begin (eval-instruction (vector-ref is (machine-pc *m*)))
                       (loop (+ n 1)))))))
    

(define (build-symbol-table is)
  (for ([ i is]
        [ id (in-range (length is))])
    (match i
      [(struct label-definition (name offset virtual?)) (hash-set! *symbol-table* name id)]
      [_ (void)])))

(define (eval-instruction i)
  (match i
    [(struct label-definition (name offset virtual?)) (next)]
    [(struct assign (reg-name value)) (write-reg reg-name value)       (next)]
    [(struct test (condition))        (push-stack condition)           (next)]
    [(struct branch (dest))           (if (pop-stack)                  (jump dest) (next))]
    [(struct goto (dest))                                              (jump dest)]
    [(struct save (reg-name))         (push-stack (read-reg reg-name)) (next)]
    [(struct restore (reg-name))      (write-reg reg-name (pop-stack)) (next)]
    [(struct perform (op))            (void (eval-mexpr op))           (next)]
    ;[(struct asm (xs))                  (for ([ x xs ]) (eval-assembly x))]
    [_                                   (error "eval-instruction: Unknown instruction" i)]))

(define (read-reg reg-name)
  (match reg-name
    ['env      (machine-env      *m*)]
    ['proc     (machine-proc     *m*)]
    ['continue (machine-continue *m*)]
    ['argl     (machine-argl     *m*)]
    ['val      (machine-val      *m*)]
    [_ (error "read-reg: Unknown register" reg-name)]))

(define (write-reg reg-name value)
  (match reg-name
    ['env      (set-machine-env!      *m* (eval-mexpr value))]
    ['proc     (set-machine-proc!     *m* (eval-mexpr value))]
    ['continue (set-machine-continue! *m* (eval-mexpr value))]
    ['argl     (set-machine-argl!     *m* (eval-mexpr value))]
    ['val      (set-machine-val!      *m* (eval-mexpr value))]
    [_ (error "write-reg: Unknown register" reg-name)]))

(define (jump dest) (set-machine-pc! *m* (eval-mexpr dest)))
(define (next) (set-machine-pc! *m* (+ 1 (machine-pc *m*))))
(define (push-stack value) (set-machine-stack! *m* (cons (eval-mexpr value))))
(define (pop-stack)
  (define val (first (machine-stack *m*)))
  (set-machine-stack! *m* (rest (machine-stack *m*)))
  val)
;(define (eval-assembly x) (undefined))
(define (eval-mexpr x)
  (match x
    [(struct reg (name))      (read-reg name)]
    [(struct const (v))       v]
    [(struct boxed-const (v)) v]
    [(struct op _)            (eval-op x)]
    [(struct label-definition (name _ _)) (hash-ref *symbol-table* name)]
    [_ (error "eval-mexpr: Unknown expression" x)]))

(define (eval-op x)
  (match x
    [(struct op ('define-variable!          (list name value env)))   (eval-op-define-variable!          name value env)]
    [(struct op ('set-variable-value!       (list name value env)))   (eval-op-set-variable-value!       name value env)]
    [(struct op ('restore-continuation!     (list cont)))             (eval-op-restore-continuation!     cont)]
    [(struct op ('box                       (list val)))              (eval-op-box                       val)]
    [(struct op ('extend-environment        (list names values env))) (eval-op-extend-environment        names values env)]
    [(struct op ('make-compiled-procedure   (list label env)))        (eval-op-make-compiled-procedure   label env)]
    [(struct op ('compiled-procedure-entry  (list proc)))             (eval-op-compiled-procedure-entry  proc)]
    [(struct op ('compiled-procedure-env    (list proc)))             (eval-op-compiled-procedure-env    proc)]
    [(struct op ('compiled-procedure?       (list proc)))             (eval-op-compiled-procedure?       proc)]
    [(struct op ('continuation?             (list proc)))             (eval-op-continuation?             proc)]
    [(struct op ('apply-primitive-procedure (list proc args)))        (eval-op-apply-primitive-procedure proc args)]
    [(struct op ('lookup-variable-value     (list name env)))         (eval-op-lookup-variable-value     name env)]
    [(struct op ('false?                    (list value)))            (eval-op-false?                    value)]
    [(struct op ('list                      (list value)))            (eval-op-list                      value)]
    [(struct op ('cons                      (list left right)))       (eval-op-cons                      left right)]
    [_ (error "eval-op: Unknown op" x)]
    ))

(define (eval-op-define-variable! name value env)
  (if (null? env)
      (eval-op-extend-environment (list name) (list value))
      (let ([ frame (first env)])
        (hash-set! frame name value))))

(define (eval-op-set-variable-value! name value env)
  (if (null? env)
      (error "eval-op-set-variable-value!: Empty environment" name value env)
      (let ([ frame (car env) ])
        (if (hash-has-key? frame name)
            (hash-set! frame name value)
            (eval-op-set-variable-value! name value (cdr env))))))

(define (eval-op-restore-continuation! cont)
  (set-machine-env! *m* (v-continuation-env))
  (jump (v-continuation-continue cont))
  )

(define (eval-op-box val)
  (match val
    [(struct v-unboxed (value)) (v-fixnum value)]
    [_ (error "eval-op-box: Should only box unboxed values")]))

(define (eval-op-extend-environment names values env)
  (cons (make-frame names values env)))

(define (eval-op-make-compiled-procedure label env)
  (v-compiled-procedure label env))

(define (eval-op-compiled-procedure-entry proc) (v-compiled-procedure-label (eval-mexpr proc)))
(define (eval-op-compiled-procedure-env proc) (v-compiled-procedure-env (eval-mexpr proc)))
(define (eval-op-compiled-procedure? proc) (v-compiled-procedure? (eval-mexpr proc)))
(define (eval-op-continuation? proc) (v-continuation? (eval-mexpr proc)))
(define (eval-op-apply-primitive-procedure proc args) (error "eval-op-apply-primitive-procedure: Unimplemented"))
(define (eval-op-lookup-variable-value name env)
  (set! env (eval-mexpr env))
  (if (null? env)
      (error "eval-op-lookup-variable-value: Variable not found" name env)
      (let ([ frame (first env) ])
        (hash-ref frame name (eval-op-lookup-variable-value name (rest env))))))

(define (eval-op-false? value)
  (set! value (eval-mexpr value))
  (if value #f #t))

(define (eval-op-list value)
  (set! value (eval-mexpr value))
  (list value))

(define (eval-op-cons left right)
  (set! left (eval-mexpr left))
  (set! right (eval-mexpr right))
  (cons left right))

(define (make-frame names values)
  (make-hash (map cons names values)))
