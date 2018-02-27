#lang typed/racket

(provide #%datum #%app #%module-begin #%top-interaction quote
         run-instructions
         verify-instructions
         (all-defined-out))

(require "globals.rkt")
(require "utils.rkt")
(require "parser.rkt")
(require (submod "types.rkt" common))
(require (submod "types.rkt" abstract-machine))
(require (submod "types.rkt" evm-assembly))

(: *m* machine)
(define *m* (machine 0
                     (v-environment null)
                     (v-compiled-procedure (label 'INVALID) (v-environment null))
                     (label 'INVALID)
                     (v-null)
                     #f
                     null
                     #f))
(: *abstract-symbol-table* (HashTable Symbol Integer))
(define *abstract-symbol-table* (make-hash))
(: *current-instruction* (Parameterof Instruction))
(define *current-instruction* (make-parameter (assign 'val (const 0))))

(module unsafe typed/racket
  (require typed/racket/unsafe)
  (require (submod "types.rkt" abstract-machine))
  (require "globals.rkt")
  (unsafe-require/typed
   "unsafe.rkt"
   [ unsafe-apply (-> Procedure values value)])
  (provide eval-op)
  (: eval-op (-> (-> MExpr value) op value))
  (define (eval-op eval-mexpr x)
    (define name (op-name x))
    (define primops (*primops*))
    (define primop (hash-ref primops name (λ () (error "eval-op: Unknown primop" name (hash-keys primops)))))
    (: e Procedure)
    (define e (primop-eval primop))
    (: args values)
    (define args (map eval-mexpr (op-args x)))
    (unsafe-apply e args)
    ))

(require 'unsafe)

(: verify-instructions (-> Instructions Void))
(define (verify-instructions is)
  is
  (void))

(: run-instructions (-> Instructions value))
(define (run-instructions is)
  (define vis (list->vector is))
  (build-symbol-table is)
  (when (verbose? VERBOSITY-LOW)
    (printf "PC\tInstruction\tStack\tval\tcontinue\tproc\targl\tenv\n"))
  (let loop ([n 0])
    (let ([pc (machine-pc *m*) ])
      (when (>= n MAX-ITERATIONS)
        (error "verify-instructions: Didn't stop after iterations" n))
      (cond ((machine-halted? *m*) (machine-val *m*))
            ((>= pc (vector-length vis)) (machine-val *m*))
            (else (begin (*current-instruction* (vector-ref vis pc))
                         (when (verbose? VERBOSITY-LOW)
                           (print-debug-line))
                         (eval-instruction (*current-instruction*))
                         (loop (+ n 1))))))))
  
(: build-symbol-table (-> Instructions Void))
(define (build-symbol-table is)
  (for ([ i is]
        [ id (in-range (length is))])
    (match i
      [(struct label-definition (name offset virtual?)) (hash-set! *abstract-symbol-table* name id)]
      [_ (void)])))

(: eval-instruction (-> Instruction Void))
(define (eval-instruction i)
  (match i
    [(struct label-definition (name offset virtual?)) (next)]
    [(struct assign (reg-name e)) (write-reg reg-name (eval-mexpr e)) (next)]
    [(struct test (condition))    (push-stack (eval-mexpr condition)) (next)]
    [(struct branch (dest))       (if (pop-stack)                     (jump (eval-mexpr dest)) (next))]
    [(struct goto (dest))                                             (jump (eval-mexpr dest))]
    [(struct save (e))            (push-stack (eval-mexpr e))         (next)]
    [(struct restore (reg-name))  (write-reg reg-name (pop-stack))    (next)]
    [(struct perform (op))        (void (eval-mexpr op))              (next)]
    [(struct evm (xs))            (for ([ x xs ]) (eval-evm x))       (next)]
    [_                               (error "eval-instruction: Unknown instruction" (*current-instruction*) i)]))

(: read-reg (-> RegisterName value))
(define (read-reg reg-name)
  (match reg-name
    ['env      (machine-env      *m*)]
    ['proc     (machine-proc     *m*)]
    ['continue (machine-continue *m*)]
    ['argl     (machine-argl     *m*)]
    ['val      (machine-val      *m*)]
    [_ (error "read-reg: Unknown register" reg-name)]))

(: write-reg (-> RegisterName value Void))
(define (write-reg reg-name v)
  (match reg-name
    ['env      (set-machine-env!      *m* v)]
    ['proc     (set-machine-proc!     *m* (cast v v-callable))]
    ['continue (set-machine-continue! *m* (cast v label))]
    ['argl     (set-machine-argl!     *m* v)]
    ['val      (set-machine-val!      *m* v)]
    [_ (error "write-reg: Unknown register" reg-name)]))

(: jump (-> value Void))
(define (jump dest)
  (assert dest label?)
  (let ([ n (label-name dest)])
    (set-machine-pc! *m* (hash-ref *abstract-symbol-table* n (λ () (error "jump: Unknown label" n))))))

(: next (-> Void))
(define (next) (set-machine-pc! *m* (+ 1 (machine-pc *m*))))

(: push-stack (-> value Void))
(define (push-stack v)
  (set-machine-stack! *m* (cons v (machine-stack *m*))))

(: pop-stack (-> value))
(define (pop-stack)
  (match (machine-stack *m*)
    [(? null?) (error "pop-stack: Attempted to pop an empty stack")]
    [(cons x xs) (set-machine-stack! *m* xs)
                 x]))

(: eval-evm (-> EthInstruction Void))
(define (eval-evm x)
  (match x
    [(struct evm-op _)              (match (eval-evm-op x)
                                      [(? void?) (void)]
                                      [x         (push-stack x)])]
    [(struct evm-push (size value)) (push-stack value)]
    [(struct label-definition _)    (void)]
    [(struct evm-bytes _)           (void)]
    [_ (error "eval-evm: Unknown syntax" x)]))

(: eval-mexpr (-> MExpr value))
(define (eval-mexpr x)
  (match x
    [(struct reg (name))      (read-reg name)]
    [(struct const (v))       (cond [(v-unboxed? v) v]
                                    [(list? v)      (list->v-list (cast v (Listof value)))]
                                    [else (error "eval-mexpr: Unhandled type" v)])]
    [(struct boxed-const (v)) (v-box v)]
    [(struct op _)            (eval-op eval-mexpr x)]
    [(struct label-definition _) x]
    [(struct %stack _)        (pop-stack)]
    [(struct evm ((list (? evm-op? i)))) (eval-evm-op i)]
    [_ (error "eval-mexpr: Unknown expression" x)]))

(: eval-op-define-variable! (-> value value value Void))
(define (eval-op-define-variable! name value env)
  (assert name symbol?)
  (assert env v-environment?)
  (let ([fs (v-environment-frames env) ])
    (if (null? fs)
        (set-machine-env! *m*
                          (eval-op-extend-environment (v-pair name (v-null))
                                                      (v-pair value (v-null)) env))
        (let* ([ frame (first fs) ]
               [ ms (v-frame-mappings frame) ])
          (hash-set! ms name value)))))

(: eval-op-set-variable-value! (-> value value value Void))
(define (eval-op-set-variable-value! name value env)
  (assert name symbol?)
  (assert env v-environment?)
  (let loop ([ fs (v-environment-frames env) ])
    (if (null? fs)
        (error "eval-op-set-variable-value!: Not found" name env)
        (let* ([ frame (first fs) ]
               [ ms (v-frame-mappings frame)])
          (if (hash-has-key? ms name)
              (hash-set! ms name value)
              (loop (rest fs)))))))

(: eval-op-restore-continuation! (-> value Void))
(define (eval-op-restore-continuation! cont)
  (assert cont v-continuation?)
  (set-machine-env! *m* (v-continuation-env cont))
  (jump (v-continuation-continue cont))
  )

(: eval-op-box (-> value value))
(define (eval-op-box val)
  (match val
    [(? exact-integer? _) (v-fixnum val)]
    [_ (error "eval-op-box: Should only box unboxed values")]))

(: eval-evm-op (-> evm-op (U Void value)))
(define (eval-evm-op x)
  (: pop-integer (-> Integer))
  (define (pop-integer)
    (: res value)
    (define res (pop-stack))
    (if (integer? res)
        res
        (error "eval-evm-op: Expected an integer" res)))
  (: binop (-> (-> Integer Integer value) value))
  (define (binop  f) (f (pop-integer) (pop-integer)))
  (: unop (-> (-> Integer value) value))
  (define (unop f) (f (pop-integer)))
  (: proc (-> Integer Void))
  (define (proc n) (begin (for ([ i (in-range n)])
                            (pop-integer))))
  (: const (-> Integer value value))
  (define (const n x) (begin (proc n)
                             x))
  (match (evm-op-name x)
    ['EQ  (binop =)]
    ['ADD (binop +)]
    ['MUL (binop *)]
    ; TODO: Everything below is a stub until we get an SMT solver or something
    ['ADDRESS 1234]
    ['CALLER  4321]
    ['CALLVALUE 0]
    ['PUSH32    0]
    ['BALANCE   (const 1 0)]
    ['CALL      (const 7 1)]
    ['SLOAD     (const 1 0)]
    ['SSTORE    (proc  2)]
    ['CALLDATALOAD (const 1 0)]
    [_ (error "eval-evm-op: Unknown EVM op" x)]
    ))

(: eval-op-extend-environment (-> value value value value))
(define (eval-op-extend-environment names values env)
  (define ns (v-list->list names))
  (define vs (v-list->list values))
  (assert env v-environment?)
  (v-environment (cons (make-frame (cast ns Symbols) vs)
                       (v-environment-frames env)))
  )

(: eval-op-make-compiled-procedure (-> value value value))
(define (eval-op-make-compiled-procedure label env)
  (assert label label?)
  (assert env v-environment?)
  (v-compiled-procedure label env))

(: eval-op-compiled-procedure-entry (-> value value))
(define (eval-op-compiled-procedure-entry proc)
  (assert proc v-compiled-procedure?)
  (v-compiled-procedure-label proc))

(: eval-op-compiled-procedure-env (-> value value))
(define (eval-op-compiled-procedure-env proc)
  (assert proc v-compiled-procedure?)
  (v-compiled-procedure-env proc))

(: eval-op-compiled-procedure? (-> value value))
(define (eval-op-compiled-procedure? proc)
  (v-compiled-procedure? proc))

(: eval-op-continuation? (-> value value))
(define (eval-op-continuation? proc) (v-continuation? proc))

(: eval-op-apply-primitive-procedure (-> value value value))
(define (eval-op-apply-primitive-procedure proc args) (error "eval-op-apply-primitive-procedure: Unimplemented"))

(: eval-op-lookup-variable-value (-> value value value))
(define (eval-op-lookup-variable-value name env)
  (assert name symbol?)
  (assert env v-environment?)
  (let loop : value ([ fs (v-environment-frames env) ])
       (if (null? fs)
           (error "eval-op-lookup-variable-value: Variable not found" (*current-instruction*) name env)
           (let ([ frame (first fs)])
             (assert frame v-frame?)
             ;(pretty-print `(DEBUG ,frame ,(hash-has-key? frame name)))
             (let ([ ms : (HashTable Symbol value) (v-frame-mappings frame)])
               (if (hash-has-key? ms name)
                   (hash-ref ms name)
                   (loop (rest fs))))))))

(: eval-op-false? (-> value value))
(define (eval-op-false? value)
  (if value #f #t))

(: eval-op-singleton (-> value value))
(define (eval-op-singleton v)
  (v-pair v (v-null)))

(: eval-op-cons (-> value value value))
(define (eval-op-cons left right)
  (v-pair left right))

(: eval-op-tag (-> value value))
(define (eval-op-tag x)
  (match x
    [(? v-unboxed? _) (error "eval-tag: Unboxed values don't have tags" x)]
    [(struct v-fixnum _)  0]
    [(struct v-symbol _)  1]
    [(struct v-compiled-procedure _) 2]
    [(struct v-primitive-procedure _) 3]
    [(struct v-pair _) 4]
    [(struct v-vector _) 5]
    [(struct v-null _) 6]
    [(struct v-continuation _) 7]
    [_ (error "eval-tag: Unknown case" x)]))

(: eval-op-allocate (-> value value))
(define (eval-op-allocate x) (undefined))

(: eval-op-read-memory (-> value * value))
(define (eval-op-read-memory . xs) (undefined))

(: eval-op-write-memory (-> value * value))
(define (eval-op-write-memory . xs) (undefined))

(: eval-op-make-fixnum (-> value value))
(define (eval-op-make-fixnum x)
  (assert x exact-integer?)
  (v-fixnum x))

(: eval-op-fixnum-value (-> value value))
(define (eval-op-fixnum-value x)
  (assert x v-fixnum?)
  (v-fixnum-value x))

(: eval-op-save-continuation (-> value))
(define (eval-op-save-continuation) (undefined))

(: eval-op-restore-continuation (-> value Void))
(define (eval-op-restore-continuation cont) (undefined))

(: eval-op-primitive-procedure? (-> value Boolean))
(define (eval-op-primitive-procedure? x) (v-primitive-procedure? x))

(: eval-op-pair? (-> value Boolean))
(define (eval-op-pair? x) (v-pair? x))

(: eval-op-pair (-> value value v-pair))
(define (eval-op-pair l r) (v-pair l r))

(: eval-op-left (-> value value))
(define (eval-op-left x)
  (assert x v-pair?)
  (v-pair-left x)
  )

(: eval-op-right (-> value value))
(define (eval-op-right x)
  (assert x v-pair?)
  (v-pair-right x)
  )

(: eval-op-null? (-> value value))
(define (eval-op-null? x) (if (v-null? x) #t #f))

(: eval-op-make-null (-> value value))
(define (eval-op-make-null x) (v-null))

(: make-frame (-> Symbols values v-frame))
(define (make-frame names values)
  (v-frame (make-hash (map (inst cons Symbol value) names values))))

(: v-list->list (-> value (Listof value)))
(define (v-list->list x)
  (match x
    [ (struct v-null _) null]
    [ (struct v-pair (l r)) (cons l (v-list->list r))]
    [ _ (error "v-list->list: Improper list detected" x)]))

(: list->v-list (-> (Listof value) value))
(define (list->v-list xs)
  (if (null? xs)
      (v-null)
      (v-pair (first xs) (list->v-list (rest xs)))))

(: v-box (-> RegisterValue value))
(define (v-box x)
  (match x
    [(? boolean? _) (error "v-box: Cannot box a boolean")]
    [(? exact-integer? _) (v-fixnum x)]
    [(? symbol? _)        (v-symbol x)]
    [(? list? _)          (v-pair (v-box (first x))
                                  (v-box (cast (rest x) RegisterValue)))]
    [_ (error "v-box: Unknown type" x)]))

(: print-debug-line (-> Void))
(define (print-debug-line)
  (printf "~a\t~v\t~v\t~v\t~v\t~v\t~v\t~v\n"
          (+ 1 (machine-pc *m*))
          (shrink-asm (*current-instruction*))
          (map shrink-value (machine-stack *m*))
          (shrink-value (machine-val      *m*))
          (shrink-value (machine-continue *m*))
          (shrink-value (machine-proc     *m*))
          (shrink-value (machine-argl     *m*))
          (shrink-value (machine-env      *m*) #:env? #t)
          ))
          
