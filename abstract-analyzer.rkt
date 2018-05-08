#lang typed/racket

(provide #%datum #%app #%module-begin #%top-interaction quote
         run-instructions
         verify-instructions
         (all-defined-out))

(require "globals.rkt")
(require "utils.rkt")
(require "expander.rkt")
(require (submod "codegen.rkt" constants))
(require (submod "types.rkt" common))
(require (submod "types.rkt" abstract-machine))
(require (submod "types.rkt" evm-assembly))
(require (submod "typed.rkt" binaryio))
(require math)

(: *m* machine)
(define *m* (machine 0 ; pc
                     (v-environment null) ; env
                     (v-compiled-procedure (label 'INVALID) (v-environment null)) ; proc
                     (label 'INVALID) ; continue
                     (v-null) ; argl
                     #f ; val
                     0 ; stack-size
                     null ; stack
                     #f)) ; halted?
(: *abstract-symbol-table* (HashTable Symbol Natural))
(define *abstract-symbol-table* (make-hash))
(: *evm-symbol-table* (HashTable Symbol Natural))
(define *evm-symbol-table* (make-hash))
(: *current-instruction* (Parameterof Instruction))
(define *current-instruction* (make-parameter (assign 'val (const 0))))
(: *current-evm-instruction* (Parameterof EthInstruction))
(define *current-evm-instruction* (make-parameter (evm-op 'UNSET)))

(: *evm-pc* (Parameterof Natural))
(define *evm-pc* (make-parameter 0))

(: *num-iterations* (Parameterof Natural))
(define *num-iterations* (make-parameter 0))

(define infinity 99999999999999)

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

(module allocator typed/racket
  (require "globals.rkt")
  (require "utils.rkt")
  (require (submod "typed.rkt" data/interval-map))
  (require (except-in (submod "types.rkt" abstract-machine) values))
  (provide (all-defined-out))

  (define-type AllocationFixnums (HashTable Natural v-fixnum))
  (define-type AllocationRanges (IntervalMap Bytes))

  (: make-allocation-ranges (-> AllocationRanges))
  (define (make-allocation-ranges) (make-interval-map))

  (: make-allocation-fixnums (-> AllocationFixnums))
  (define (make-allocation-fixnums) (make-hash))

  (: *allocation-ranges* (Parameterof AllocationRanges))
  (define *allocation-ranges* (make-parameter (make-allocation-ranges)))

  (: *allocation-ptr* (Parameterof Natural))
  (define *allocation-ptr* (make-parameter 1337))

  (: *allocation-fixnums* (Parameterof AllocationFixnums))
  (define *allocation-fixnums* (make-parameter (make-allocation-fixnums)))

  (: tick-allocator! (-> Natural Void))
  (define (tick-allocator! size)
    (*allocation-ptr* (+ (*allocation-ptr*) size ALLOCATION-RANGE-PADDING)))

  (: allocate-range! (-> Natural Natural))
  (define (allocate-range! len)
    (define ptr (*allocation-ptr*))
    (interval-map-set! (*allocation-ranges*) ptr (+ ptr len) (make-bytes len))
    (tick-allocator! len)
    ptr
    )

  (: pad-num-bytes (-> Natural Natural))
  (define (pad-num-bytes x)
    (* WORD (ceiling (/ x WORD))))

  (: allocate-bytes! (-> Bytes Natural))
  (define (allocate-bytes! ibs)
    (define ptr (allocate-range! (pad-num-bytes (bytes-length ibs))))
    (let-values ([(_ obs) (get-allocation ptr 0)])
      (bytes-copy! obs 0 ibs))
    ptr)

  (: get-allocation (-> Natural Natural (Values Natural Bytes)))
  (define (get-allocation ptr os)
    (let-values ([(start end bs) (interval-map-ref/bounds (*allocation-ranges*)
                                                          ptr)])
      (values (+ (- ptr start) (* os WORD))
              bs)
      ))

  (: make-v-fixnum (-> Natural v-fixnum))
  (define (make-v-fixnum val)
    (define ptr (*allocation-ptr*))
    (define fx (v-fixnum val ptr))
    (hash-set! (*allocation-fixnums*) ptr fx)
    (tick-allocator! WORD)
    fx
    )

  (: get-allocated-fixnum (-> Integer (U #f v-fixnum)))
  (define (get-allocated-fixnum addr)
    (if (natural? addr)
        (hash-ref (*allocation-fixnums*) addr #f)
        #f))
  )

(require 'allocator)

(: verify-instructions (-> Instructions Void))
(define (verify-instructions is)
  is
  (void))

(: run-instructions (-> Instructions value))
(define (run-instructions is)
  (define vis (list->vector is))
  (build-abstract-symbol-table is)
  (when (verbose? VERBOSITY-LOW)
    (printf "PC\tInstruction\tStack\tval\tcontinue\tproc\targl\tstack-size\tenv\tallocated ranges\tallocated words\n"))
  (parameterize ([ *num-iterations* 0])
    (let loop ()
      (let ([pc (machine-pc *m*) ])
        (tick-iteration!)
        (cond ((machine-halted? *m*) (machine-val *m*))
              ((>= pc (vector-length vis)) (machine-val *m*))
              (else (begin (*current-instruction* (vector-ref vis pc))
                           (when (verbose? VERBOSITY-LOW)
                             (print-debug-line))
                           (eval-instruction (*current-instruction*))
                           (loop))))))))

(: tick-iteration! (-> Void))
(define (tick-iteration!)
  (define n (*num-iterations*))
  (define max-iterations (*max-simulation-steps*))
  (when (>= n max-iterations)
    (error "tick-iteration!: Didn't stop after iterations" n))
  (*num-iterations* (+ 1 n)))

(: build-symbol-table (All (A) (-> (Listof (U A label-definition)) (HashTable Symbol Natural))))
(define (build-symbol-table is)
  (: ret (HashTable Symbol Natural))
  (define ret (make-hash))
  (for ([i is]
        [ id : Natural (in-range (length is))])
    (match i
      [(struct label-definition (name offset virtual?)) (hash-set! ret name id)]
      [_ (void)]))
  ret)

(: build-abstract-symbol-table (-> Instructions Void))
(define (build-abstract-symbol-table is)
  (set! *abstract-symbol-table* (build-symbol-table is)))

(: build-evm-symbol-table (-> EthInstructions Void))
(define (build-evm-symbol-table ethis)
  (set! *evm-symbol-table* (build-symbol-table ethis)))

(: eval-instruction (-> Instruction Void))
(define (eval-instruction i)
  (match i
    [(struct label-definition (name offset virtual?)) (next)]
    [(struct assign (reg-name e)) (write-reg reg-name (eval-mexpr e)) (next)]
    [(struct test (condition))    (push-stack (eval-mexpr condition)) (next)]
    [(struct branch (dest))       (if (value->bool (pop-stack))       (jump (eval-mexpr dest)) (next))]
    [(struct goto (dest))                                             (jump (eval-mexpr dest))]
    [(struct save (e))            (push-stack (eval-mexpr e))         (next)]
    [(struct restore (reg-name))  (write-reg reg-name (pop-stack))    (next)]
    [(struct perform (op))        (void (eval-mexpr op))              (next)]
    [(struct evm (xs))            (eval-evms xs)                      (next)]
    [_                               (error "eval-instruction: Unknown instruction" (*current-instruction*) i)]))

(: read-reg (-> RegisterName value))
(define (read-reg reg-name)
  (match reg-name
    ['env        (machine-env        *m*)]
    ['proc       (machine-proc       *m*)]
    ['continue   (machine-continue   *m*)]
    ['argl       (machine-argl       *m*)]
    ['val        (machine-val        *m*)]
    ['stack-size (machine-stack-size *m*)]
    [_ (error "read-reg: Unknown register" reg-name)]))

(: write-reg (-> RegisterName value Void))
(define (write-reg reg-name v)
  (match reg-name
    ['env        (set-machine-env!        *m* (cast v v-environment))]
    ['proc       (set-machine-proc!       *m* (cast v v-callable))]
    ['continue   (set-machine-continue!   *m* (cast v label))]
    ['argl       (set-machine-argl!       *m* v)]
    ['val        (set-machine-val!        *m* v)]
    ['stack-size (begin
                   (assert v natural?)
                   (set-machine-stack-size! *m* v)
                   (let ([ size (length (machine-stack *m*))])
                     (unless (= size v)
                       (error "write-reg: Attempted to write an incorrect stack size" v size))))]

    [_ (error "write-reg: Unknown register" reg-name)]))

(: jump (-> value Void))
(define (jump dest)
  (assert dest label?)
  (let ([ n (label-name dest)])
    (set-machine-pc! *m* (hash-ref *abstract-symbol-table* n (λ () (error "jump: Unknown label" n))))))

(: jump-evm (-> value Void))
(define (jump-evm dest)
  (assert dest label?)
  (let ([ n (label-name dest)])
    (*evm-pc* (hash-ref *evm-symbol-table* n (λ () (error "jump-evm: Unknown label" n))))))

(: next (-> Void))
(define (next) (set-machine-pc! *m* (+ 1 (machine-pc *m*))))

(: evm-next (-> Void))
(define (evm-next) (*evm-pc* (+ 1 (*evm-pc*))))

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
    [_ (error "eval-evm: Unknown syntax" x)]
    )
  (evm-next))

(: eval-evms (-> EthInstructions Void))
(define (eval-evms xs)
  (define is (list->vector xs))
  (build-evm-symbol-table xs)

  (parameterize ([ *evm-pc* 0])
    (let loop ([ n 0 ])
      (tick-iteration!)
      (if (or (>= n (vector-length is))
              (<  n 0))
          (void)
          (begin
            (parameterize ([ *current-evm-instruction* (vector-ref is n)])
              (eval-evm (*current-evm-instruction*))
              (when (verbose? VERBOSITY-LOW)
                (print-debug-line))
              (loop (*evm-pc*))))))))

(: eval-mexpr (-> MExpr value))
(define (eval-mexpr x)
  (match x
    [(struct reg (name))      (read-reg name)]
    [(struct const (v))       (cond [(v-unboxed? v) v]
                                    [(list? v)      (list->v-list (cast v (Listof value)))]
                                    [(char? v)      (char->integer v)]
                                    [(string? v)    (v-bytes (allocate-bytes! (string->bytes/utf-8 v)))]
                                    [else (error "eval-mexpr: Unhandled type" v)])]
    [(struct boxed-const (v)) (v-box v)]
    [(struct op _)            (eval-op eval-mexpr x)]
    [(struct label-definition _) x]
    [(struct %stack _)        (pop-stack)]
    [(struct evm ((list (? evm-op? i)))) (eval-evm-op i)]
    [(struct label _) x]
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
    [(? natural? _) (make-v-fixnum val)]
    [_ (error "eval-op-box: Should only box unboxed values")]))

(: eval-evm-op (-> evm-op (U Void value)))
(define (eval-evm-op x)
  (: pop-integer (-> Integer))
  (define (pop-integer)
    (match (pop-stack)
      [(? integer? x) x]
      [(struct v-bytes (x)) x]
      [_ (error "eval-evm-op: Expected an integer or bytes")]
      ))
  (: pop-integer* (-> Integer))
  (define (pop-integer*)
    (match (pop-stack)
      [(? integer? x) x]
      [(? boolean? x) (if x 1 0)]
      [(? symbol?  x) (symbol->integer x)]
      [x (error "eval-evm-op: Expected an unboxed value" x)]
      ))
  (: overflow-value (-> value value))
  (define (overflow-value x)
    (if (exact-integer? x)
        (truncate-int x)
        x))
  (: binop (-> (-> Integer Integer value) value))
  (define (binop  f)
    (let* ([ a (pop-integer) ]
           [ b (pop-integer) ])
      (overflow-value (f a b))))
  (: binop* (-> (-> Integer Integer value) value))
  (define (binop* f)
    (let* ([ a (pop-integer*) ]
           [ b (pop-integer*) ])
      (overflow-value (f a b))))
  (: unop (-> (-> Integer value) value))
  (define (unop f) (f (pop-integer)))
  (: unop* (-> (-> Integer value) value))
  (define (unop* f) (f (pop-integer*)))

  (: proc (-> Integer Void))
  (define (proc n) (begin (for ([ i (in-range n)])
                            (pop-integer))))
  (: const (-> Integer value value))
  (define (const n x) (begin (proc n)
                             x))
  (match (evm-op-name x)
    ['EQ  (binop* =)]
    ['GT  (binop >)]
    ['LT  (binop <)]
    ['SLT (binop >)] ; TODO: Signed < should differ from unsigned <
    ['SGT (binop <)]
    ['AND (binop* bitwise-and)]
    ['OR  (binop* bitwise-ior)]
    ['XOR (binop* bitwise-xor)]
    ['NOT (unop*  (compose truncate-int bitwise-not))]
    ['EXP (binop (λ (a b) (modular-expt a b WORDLIMIT)))]
    ['ADD (binop +)]
    ['SUB (binop -)]
    ['MUL (binop *)]
    ['ISZERO (match (pop-stack) [#t #f] [#f #t] [0 1] [(? integer?) 0])]
    ['DIV    (binop (λ (a b) (if (equal? 0 b) 0 (exact-floor (/ a b)))))]
    ['MOD    (binop (λ (a b) (if (equal? 0 b) 0 (modulo a b))))]
    ['JUMP (jump-evm (pop-stack))]
    ['LOG0 (proc 2)]
    ['LOG1 (proc 3)]
    ['LOG2 (proc 4)]
    ['LOG3 (proc 5)]
    ['LOG4 (proc 6)]
    ['REVERT (set-machine-halted?! *m* #t)]
    ['SIGNEXTEND (binop (λ (a b)
                          (assert b natural?)
                          (sign-extend (truncate-int a) b)))]
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

(: eval-op-extend-environment (-> value value value v-environment))
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
  (if (value->bool value) #f #t))

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
    [(struct v-fixnum              _) 0]
    [(struct v-symbol              _) 1]
    [(struct v-compiled-procedure  _) 2]
    [(struct v-primitive-procedure _) 3]
    [(struct v-pair                _) 4]
    [(struct v-vector              _) 5]
    [(struct v-null                _) 6]
    [(struct v-continuation        _) 7]
    [(struct v-frame               _) 8]
    [(struct v-environment         _) 9]
    [(struct v-char                _) 10]
    [(struct v-bytes               _) 11]
    [_ (error "eval-tag: Unknown case" x)]))

(: eval-op-allocate (-> value value))
(define (eval-op-allocate size)
  (assert size natural?)
  (allocate-range! (* size WORD))
  )

(: eval-op-read-memory (-> value value value))
(define (eval-op-read-memory ptr os)
  (: ptr EthWord)
  (define ptr-val (eval-op-any->unboxed ptr))
  (with-asserts ([ os natural?])
    (match (get-allocated-fixnum (+ ptr-val os))
      [#f (let*-values ([(os2 bs) (get-allocation ptr-val os) ]
                        [(word)   (bytes->integer bs #f #t os2 (+ os2 WORD))])
            word)]
      [fx fx])))

(: eval-op-write-memory (-> value value value value))
(define (eval-op-write-memory ptr os val)
  (define word-val (eval-op-any->unboxed val))
  (with-asserts ([ ptr natural? ]
                 [ os  natural? ])
    (match (get-allocated-fixnum (- ptr WORD))
      [#f (let-values ([(os2 bs) (get-allocation ptr os)])
            ; If (bytes-copy!) triggers an exception, on the EVM this would instead corrupt nearby memory.
            (bytes-copy! bs os2 (integer->bytes (truncate-int word-val) 32 #f #t)))]
      [(? v-fixnum? fx) (set-v-fixnum-value! fx word-val)])
    ))

(: eval-op-make-fixnum (-> value value))
(define (eval-op-make-fixnum x)
  (match x
    [(? exact-nonnegative-integer?) (make-v-fixnum x)]
    [(? v-fixnum?) (make-v-fixnum (v-fixnum-ptr x))]
    [_ (error "eval-op-make-fixnum: Unknown case" x)]))

(: eval-op-fixnum-value (-> value value))
(define (eval-op-fixnum-value x)
  (assert x v-fixnum?)
  (v-fixnum-value x))

(: eval-op-save-continuation (-> value))
(define (eval-op-save-continuation)
  (: cont v-continuation)
  (define cont (v-continuation (machine-continue *m*)
                               (machine-env      *m*)
                               (machine-stack    *m*)))
  cont
  )

(: eval-op-restore-continuation (-> value Void))
(define (eval-op-restore-continuation cont)
  (assert cont v-continuation?)
  (jump (v-continuation-continue cont))
  (set-machine-env! *m* (v-continuation-env cont))
  (set-machine-stack! *m* (v-continuation-stack cont))
  )

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

(: eval-op-make-null (-> value))
(define (eval-op-make-null) (v-null))

(: eval-op-bool->unboxed (-> value value))
(define (eval-op-bool->unboxed x)
  (match x
    [#t 1]
    [#f 0]
    [_ (error "eval-op-bool->unboxed: Expected a boolean")]))

(: eval-op-add (-> value value value))
(define (eval-op-add a b)
  (assert a exact-integer?)
  (assert b exact-integer?)
  (+ a b))

(: make-frame (-> Symbols values v-frame))
(define (make-frame names values)
  (v-frame (make-hash (map (inst cons Symbol value) names values))))

(: v-list->list (-> value (Listof value)))
(define (v-list->list x)
  (match x
    [ (struct v-null _) null]
    [ (struct v-pair (l (? v-list? r))) (cons l (v-list->list r))]
    [ (struct v-pair (l r)) (list l r)]
    [ _ (error "v-list->list: Unknown list type" x)]))

(: list->v-list (-> (Listof value) value))
(define (list->v-list xs)
  (if (null? xs)
      (v-null)
      (v-pair (first xs) (list->v-list (rest xs)))))

(: v-box (-> RegisterValue value))
(define (v-box x)
  (match x
    [(? boolean?) (error "v-box: Cannot box a boolean")]
    [(? exact-nonnegative-integer?) (make-v-fixnum x)]
    [(? exact-integer?)             (make-v-fixnum (truncate-int x))]
    [(? symbol?)        (v-symbol x)]
    [(? list?)          (v-pair (v-box (first x))
                                (v-box (cast (rest x) RegisterValue)))]
    [(? vector?)        (v-vector (vector-map v-box x))]
    [(? char?)          (v-char x)]
    [(? string?)        (let ([ bs (string->bytes/utf-8 x) ])
                          (v-bytes (allocate-bytes! (bytes-append (word->bytes TAG-BYTES)
                                                                  (word->bytes (bytes-length bs))
                                                                  bs))))]
    [_ (error "v-box: Unknown type" x)]))

(: print-debug-line (-> Void))
(define (print-debug-line)
  (define pc (+ 1 (machine-pc *m*)))
  (printf "~a\t~v\t~v\t~v\t~v\t~v\t~v\t~v\t~v\t~v\t~v\n"
          (match (*evm-pc*)
            [-1 pc]
            [x  (format "~a:~a" pc (*evm-pc*))])
          (asm->datum (*current-instruction*))
          (map value->datum (machine-stack *m*))
          (value->datum (machine-val      *m*))
          (value->datum (machine-continue *m*))
          (value->datum (machine-proc     *m*))
          (value->datum (machine-argl     *m*))
          (value->datum (machine-stack-size *m*))
          (value->datum (machine-env      *m*) #:env? (verbose? VERBOSITY-HIGH))
          (if (verbose? VERBOSITY-MEDIUM) (*allocation-ranges*) '())
          (if (verbose? VERBOSITY-MEDIUM) (*allocation-fixnums*) '())
          ))

(: v-list->environment (-> v-list v-environment))
(define (v-list->environment xs)
  (match xs
    [(struct v-null _) (v-environment null)]
    [(struct v-pair ((struct v-pair (names values))
                     (? v-list? next)))
     (eval-op-extend-environment names values (v-list->environment next))]
    [_ (error "v-list->environment: Could not parse as an environment" xs)]
    ))

(: eval-op-vector-read (-> value value value))
(define (eval-op-vector-read vec os)
  (assert vec v-vector?)
  (assert os  exact-integer?)
  (vector-ref (v-vector-elems vec) os))

(: eval-op-vector-write (-> value value value Void))
(define (eval-op-vector-write vec os x)
  (assert vec v-vector?)
  (assert os  exact-integer?)
  (vector-set!  (v-vector-elems vec) os x))

(: eval-op-word->pointer (-> value value))
(define (eval-op-word->pointer x)
  (assert x exact-integer?)
  x
  )

(: eval-op-symbol-value (-> value value))
(define (eval-op-symbol-value x)
  (assert x v-symbol?)
  (symbol->integer (v-symbol-value x))
  )

(: eval-op-character-value (-> value value))
(define (eval-op-character-value x)
  (assert x v-char?)
  (char->integer (v-char-value x))
  )

(: value->bool (-> value Boolean))
(define (value->bool x)
  (match x
    [0  #f]
    [#f #f]
    [x  #t]
    ))

(: eval-op-any->unboxed (-> value EthWord))
(define (eval-op-any->unboxed x)
  (match x
    [(? natural?)           x]
    [(? exact-integer?)     (truncate-int x)]
    [(? symbol?)            (symbol->integer x)]
    [(struct v-bytes (ptr)) ptr]
    [_ (error "eval-op-any->unboxed: Unsupported type" x)]
    ))
