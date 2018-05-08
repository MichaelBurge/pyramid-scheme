#lang typed/racket

(provide on-simulate-debug
         on-log-print)

(require (submod "types.rkt" common))
(require (submod "types.rkt" simulator))
(require (submod "types.rkt" abstract-machine))
(require (submod "types.rkt" evm-assembly))
(require "expander.rkt")
(require "globals.rkt")
(require "io.rkt")
(require "serializer.rkt")
(require "simulator.rkt")
(require "codegen.rkt")
(require "utils.rkt")
(require "abstract-analyzer.rkt")
(require (submod "typed.rkt" binaryio))
(require (submod "typed.rkt" data/interval-map))
(require (submod "typed.rkt" json))

(: on-simulate-debug (-> SymbolTable SourceMap OnSimulateCallback))
(define (on-simulate-debug symbol-table sourcemap)
  (: debug-header? Boolean)
  (define debug-header? #f)
  (: sourcemapper SourceMapper)
  (define sourcemapper (make-evm-sourcemapper symbol-table sourcemap))
  (: reverse-symbol-table ReverseSymbolTable)
  (define reverse-symbol-table (invert-hash symbol-table))
  (λ (vm i reads)
    (parameterize ([ *warnings?* #f])
      (unless debug-header?
        (set! debug-header? #t)
        (printf "PC\tSourcemap\tSymbol\tInstruction\tStack\tval\tcontinue\tproc\targl\tstack-size\tenv\tStack(uninterpreted)\tmemory\n"))
      (printf "~a\t~a\t~a\t~v\t~v\t~v\t~v\t~v\t~v\t~v\t~v\t~v\t~a\n"
              (integer->hex (vm-exec-pc vm))
              (lookup-evm-sourcemap-string sourcemapper (vm-exec-pc vm))
              (reverse-symbol-name reverse-symbol-table (vm-exec-pc vm))
              (evm->datum i)
              (map (λ ([ ptr : EthWord ]) (value->datum (vm-value vm ptr)))
                   (vm-exec-stack vm))
              (value->datum (vm-register vm 'val))
              (value->datum (vm-register vm 'continue))
              (value->datum (vm-register vm 'proc))
              (value->datum (vm-register vm 'argl))
              (value->datum (vm-register vm 'stack-size))
              (value->datum (vm-register vm 'env) #:env? #t)
              (jsexpr->string (vm-exec-stack vm))
              (if (verbose? VERBOSITY-MEDIUM)
                  (jsexpr->string (memory-dict vm))
                  "")
              ))))

(: on-log-print (-> vm-exec Bytes EthWords Void))
(define (on-log-print vm bs ts)
  (: ioctl? (-> Any Boolean))
  (define (ioctl? t) (equal? t IOCTL-TAG))
  (match ts
    [(list-no-order (? ioctl?) t) (handle-ioctl vm bs (cast t EthWord))]
    [_ (println `(,ts ,bs))]
    ))

(: handle-ioctl (-> vm-exec Bytes EthWord Void))
(define (handle-ioctl vm bs t)
  (match t
    [0 (println `(LOGNUM ,(bytes->integer bs #f)))]
    [1 (println `(LOGSYM ,(string->symbol (integer->string (bytes->integer bs #f)))))]
    [2 (println `(LOGSTR ,bs))]
    [3 (write-char (integer->char (bytes->integer bs #f)))]
    ))

(: memory-dict (-> vm-exec (Listof (List UnlinkedOffset EthWord))))
(define (memory-dict vm)
  (for/list : (Listof (List UnlinkedOffset EthWord))
      ([ i : MemoryOffset (in-range 0 (vm-exec-largest-accessed-memory vm) 32) ])
    (list i (read-memory-word vm i 32))))


; TODO: Incorrect if x is a null value that isn't the shared copy of nil.

(: vm-tag (-> vm-exec EthWord EthWord))
(define (vm-tag vm ptr) (read-memory-word vm ptr 32))

(: vm-register (-> vm-exec RegisterName value))
(define (vm-register vm name)
  (define unboxed (read-memory-word vm (register-address name) WORD))
  (match name
    ['env        (vm-environment vm unboxed)]
    ['proc       (vm-procedure vm unboxed)]
    ['continue   (vm-label vm unboxed)]
    ['argl       (vm-value vm unboxed)]
    ['val        (vm-value vm unboxed)]
    ['stack-size unboxed]
    [_ (error "vm-register: Unknown register" name)]))

(define-syntax-rule (tick-recursion default eval)
  (let ([ depth (*recursion-depth*) ])
    (if (<= depth 0)
        default
        (parameterize ([ *recursion-depth* (- depth 1)])
          eval))))

(: vm-value (-> vm-exec EthWord value))
(define (vm-value vm ptr)
  (tick-recursion
   ptr
   (cond
     [ (>= ptr 65536)                   (string->symbol (integer->string ptr))]
     [ (>= ptr (vm-exec-largest-accessed-memory vm)) ptr]
     [ (<= ptr MEM-ALLOCATOR)           ptr ]
     [ (vm-fixnum? vm ptr)              (vm-fixnum vm ptr)]
     [ (vm-symbol? vm ptr)              (vm-symbol vm ptr)]
     [ (vm-compiled-procedure? vm ptr)  (vm-compiled-procedure vm ptr)]
     [ (vm-primitive-procedure? vm ptr) (vm-primitive-procedure vm ptr)]
     [ (vm-pair? vm ptr)                (vm-pair vm ptr)]
     [ (vm-vector? vm ptr)              (vm-vector vm ptr)]
     [ (vm-null? vm ptr)                (v-null)]
     [ (vm-frame? vm ptr)               (vm-frame vm ptr)]
     [ (vm-environment? vm ptr)         (vm-environment vm ptr)]
     [ else                             ptr]
     )))

(define-type TagChecker (-> vm-exec EthWord Boolean))

(: make-tag-checker (-> EthWord TagChecker))
(define (make-tag-checker tag) (λ (vm ptr) (equal? tag (vm-tag vm ptr))))

(: vm-fixnum? TagChecker)
(define vm-fixnum? (make-tag-checker TAG-FIXNUM))

(: vm-symbol? TagChecker)
(define vm-symbol? (make-tag-checker TAG-SYMBOL))

(: vm-compiled-procedure? TagChecker)
(define vm-compiled-procedure? (make-tag-checker TAG-COMPILED-PROCEDURE))

(: vm-primitive-procedure? TagChecker)
(define vm-primitive-procedure? (make-tag-checker TAG-PRIMITIVE-PROCEDURE))

(: vm-pair? TagChecker)
(define vm-pair? (make-tag-checker TAG-PAIR))

(: vm-vector? TagChecker)
(define vm-vector? (make-tag-checker TAG-VECTOR))

(: vm-null? TagChecker)
(define vm-null? (make-tag-checker TAG-NIL))

(: vm-frame? TagChecker)
(define vm-frame? (make-tag-checker TAG-FRAME))

(: vm-environment? TagChecker)
(define vm-environment? (make-tag-checker TAG-ENVIRONMENT))

(: vm-frame (-> vm-exec EthWord v-frame))
(define (vm-frame vm ptr)
  (define l-ptr (read-memory-word vm (+ ptr #x20) WORD))
  (define r-ptr (read-memory-word vm (+ ptr #x40) WORD))
  (match* ((vm-value vm l-ptr) (vm-value vm r-ptr))
    [((? v-list? l) (? v-list? r))
     (let ([ maps : (Listof (Pairof Symbol value))
                  (for/list ([ name  (v-list->list l)]
                             [ value (v-list->list r)]
                             #:when (symbol? name))
                    (cons name value))])
       (v-frame (make-hash maps)))]
    [(_ _) (v-frame (make-hash))]
    ))

(: vm-fixnum (-> vm-exec EthWord v-fixnum))
(define (vm-fixnum vm ptr)
  (v-fixnum (read-memory-word vm (+ ptr #x20) 32) ptr))

(: vm-symbol (-> vm-exec EthWord v-symbol))
(define (vm-symbol vm ptr)
  (: word EthWord)
  (define word (read-memory-word vm (+ ptr #x20) 32))
  (v-symbol (string->symbol (integer->string word))))

(: vm-label (-> vm-exec UnlinkedOffset Symbol))
(define (vm-label vm addr)
  (string->symbol
   (string-append
    "label-"
    (symbol->string (hash-ref (*reverse-symbol-table*)
                              addr
                              (λ () 'ERROR))))))

(: vm-procedure (-> vm-exec EthWord value))
(define (vm-procedure vm ptr)
  (vm-label vm (read-memory-word vm (+ ptr WORD) WORD)))

(define vm-compiled-procedure vm-procedure)
(define vm-primitive-procedure vm-procedure)

(: vm-pair (-> vm-exec EthWord v-pair))
(define (vm-pair vm ptr)
  (define l-ptr (read-memory-word vm (+ ptr #x20) WORD))
  (define r-ptr (read-memory-word vm (+ ptr #x40) WORD))
  (v-pair (vm-value vm l-ptr)
          (vm-value vm r-ptr)))

(: vm-vector (-> vm-exec EthWord v-vector))
(define (vm-vector vm x)
  (let* ([ addr (read-memory-word vm (+ x #x40) WORD)]
         [ len  (read-memory-word vm (+ x #x20) WORD)]
         [ end  (+ addr len)]
         )
    (v-vector
     (for/vector : (Vectorof value) ([ i : Natural (in-range addr end WORD)])
       (vm-value vm i)))))

(: vm-environment (-> vm-exec EthWord v-environment))
(define (vm-environment vm ptr)
  (define frame-ptr (read-memory-word vm (+ ptr #x20) WORD))
  (define env-ptr   (read-memory-word vm (+ ptr #x40) WORD))
  (define env-null  (v-environment null))
  (if (vm-null? vm env-ptr)
      env-null
      (tick-recursion env-null
                      (v-environment (cons (vm-frame vm frame-ptr)
                                           (v-environment-frames (vm-environment vm env-ptr)))))))

(: to-json-compatible (-> Any Any))
(define (to-json-compatible x)
  (match x
    [(? symbol? x) (string-append "'" (symbol->string x))]
    [(? list? x) (map to-json-compatible x)]
    [(cons a b)  (list (to-json-compatible a) (to-json-compatible b))]
    [_           x]
    ))

(: lookup-evm-sourcemap-string (-> SourceMapper UnlinkedOffset String))
(define (lookup-evm-sourcemap-string mapper os)
  (: syms Symbols)
  (define syms (mapper os))
  (string-join (map symbol->string syms)))

(: make-evm-sourcemapper (-> SymbolTable SourceMap SourceMapper))
(define (make-evm-sourcemapper syms srcs)
  (define infinity #xFFFFFFFF)
  (: m (IntervalMap Symbols))
  (define m (make-interval-map))
  (interval-map-set! m 0 infinity '(UNKNOWN))
  (for ([ sym (sorted-symbol-table syms) ])
    (let ([ label (car sym) ]
          [ os    (cdr sym) ])
      (interval-map-set! m os infinity (hash-ref srcs label (λ () '(UNKNOWN))))))
  (λ ([ os : UnlinkedOffset ])
    (interval-map-ref m os)))
