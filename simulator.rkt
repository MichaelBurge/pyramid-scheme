#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "disassembler.rkt")
(require "serializer.rkt")
(require "codegen.rkt")
(require "globals.rkt")
(require binaryio/integer)
(require json)

(provide
 make-txn-create
 make-txn-message
 apply-txn-create!
 apply-txn-message!
 ;
 infer-type
 parse-type
 ;
 *on-simulate-instruction*
 on-simulate-nop
 on-simulate-debug
 )

(: apply-txn-create! (-> simulator vm-txn simulation-result-ex))
(define (apply-txn-create! sim txn)
  (let ([ vm (make-vm-exec sim (vm-txn-data txn)) ])
    (with-handlers ([ exn:evm:return? (λ (x)
                                        (let* ([ bs (exn:evm:return-result x )]
                                               [ receipt (vm->receipt vm bs) ]
                                               [ addr (vm-txn-receipt-contract-address receipt )]
                                               )
                                          (install-bytecode! sim addr bs)
                                          (simulation-result vm bs receipt)))])
      (simulate! vm MAX-ITERATIONS))
    ))

(: apply-txn-message! (-> simulator vm-txn simulation-result-ex))
(define (apply-txn-message! sim txn)
  
  (let* ([ bytecode (lookup-bytecode sim (vm-txn-to txn))]
         [ vm (make-vm-exec sim bytecode)])
    (with-handlers ([ exn:evm:return? (λ (x) (simulation-result vm (exn:evm:return-result x) (vm->receipt vm x)))]
                    [ exn:evm? (λ (x) x)])
      (simulate! vm MAX-ITERATIONS))))

(define (on-simulate-nop vm i reads) (void))
(define *on-simulate-instruction* (make-parameter on-simulate-nop))
(define *nonce* (make-parameter 0))

(define MEMORY-SIZE 200000)
(define MAX-ITERATIONS 1000000)
(define DEFAULT-GAS-PRICE 10)
(define DEFAULT-GAS-LIMIT 1000000)

; TODO: Contract address should use actual Ethereum spec rather than a counter
(define *contract-create-counter* (make-parameter 0))

; Appendix G in Ethereum Yellow Paper: http://gavwood.com/paper.pdf
(define G_zero          0)
(define G_base          2)
(define G_verylow       3)
(define G_low           5)
(define G_mid           8)
(define G_high          10)
(define G_extcode       700)
(define G_balance       400)
(define G_sload         200)
(define G_jumpdest      1)
(define G_sset          20000)
(define G_sreset        5000)
(define R_sclear        15000)
(define R_suicide       24000)
(define G_suicide       5000)
(define G_create        32000)
(define G_codedeposit   200)
(define G_call          700)
(define G_callvalue     9000)
(define G_callstipend   2300)
(define G_newaccount    25000)
(define G_exp           10)
(define G_expbyte       10)
(define G_memory        3)
(define G_txcreate      32000)
(define G_txdatazero    4)
(define G_txdatanonzero 68)
(define G_transaction   21000)
(define G_log           375)
(define G_logdata       8)
(define G_logtopic      375)
(define G_sha3          30)
(define G_sha3word      6)
(define G_copy          3)
(define G_blockhash     20)

(define dups '(DUP1 DUP2 DUP3 DUP4 DUP5 DUP6 DUP7 DUP8 DUP9 DUP10 DUP11 DUP12 DUP13 DUP14 DUP15 DUP16))
(define swaps '(SWAP1 SWAP2 SWAP3 SWAP4 SWAP5 SWAP6 SWAP7 SWAP8 SWAP9 SWAP10 SWAP11 SWAP12 SWAP13 SWAP14 SWAP15 SWAP16))

(define W_zero '(STOP RETURN))
(define W_base '(ADDRESS ORIGIN CALLER CALLVALUE CALLDATASIZE CODESIZE GASPRICE COINBASE TIMESTAMP NUMBER DIFFICULTY GASLIMIT POP PC MSIZE GAS))
(define W_verylow '(ADD SUB NOT LT GT SLT SGT EQ ISZERO AND OR XOR BYTE CALLDATALOAD MLOAD MSTORE MSTORE8))
(define W_low '(MUL DIV SDIV MOD SMOD SIGNEXTEND))
(define W_mid '(ADDMOD MULMOD JUMP))
(define W_high '(JUMPI))
(define W_extcode '(EXTCODESIZE))
                    
(: vm->receipt (-> vm-exec Bytes txn-receipt))
(define (vm->receipt vm bs) 
  (vm-txn-receipt (simulator-world (vm-exec-sim vm)) ; post-transaction world
                  (vm-exec-gas vm)                   ; cumulative gas
                  'undefined                         ; log bloom
                  '()                                ; logs list
                  (tick-counter! *contract-create-counter*) ; contract address
                  )
  )
  
(: make-vm-exec (-> simulation Bytes vm-exec))
(define (make-vm-exec sim bytes)
  (vm-exec 1     ; step
           bytes ; bytecode
           0     ; pc
           null  ; stack
           (make-bytes MEMORY-SIZE) ; memory
           0     ; gas
           0     ; largest memory access
           sim
           ))

(: simulate! (-> vm-exec Fixnum Void))
(define (simulate! vm max-iterations)
  (if (<= max-iterations 0)
      (raise (exn:evm:did-not-halt "run-until-return" (current-continuation-marks) vm max-iterations))
      (begin
        (simulate-one! vm (next-instruction vm))
        (simulate! vm (- max-iterations 1)))))

(: instruction-at (-> vm-exec Fixnum EthInstruction))
(define (instruction-at vm addr)
  (cdr (disassemble-one (vm-exec-bytecode vm) addr)))

(: next-instruction (-> vm-exec EthInstruction))
(define (next-instruction vm)
  (instruction-at vm (vm-exec-pc vm)))
  
(: simulate-one! (-> vm-exec EthInstruction Void))
(define (simulate-one! vm i)
  (let* ([ used-gas (instruction-gas vm i) ]
         [ op       (eth-to-opcode i) ]
         [ stk      (vm-exec-stack vm) ]
         [ num-reads (opcode-num-reads op) ]
         [ reads (if (>= (length stk) num-reads)
                     (take stk num-reads)
                     (raise (exn:evm:stack-underflow "simulate-one!" (current-continuation-marks) vm num-reads stk)))]
         )
    ((*on-simulate-instruction*) vm i reads)
    (cond ((label? i)    (simulate-nop!  vm))
          ((eth-push? i) (simulate-push! vm (eth-push-size i) (eth-push-value i)))
          ((eth-asm? i)  (simulate-asm!  vm (eth-asm-name i)))
          (else
           (error "Unknown opcode found - simulate-one:" i)))
    (set-vm-exec-pc!   vm (+ (vm-exec-pc   vm) (instruction-size i)))
    (set-vm-exec-gas!  vm (+ (vm-exec-gas  vm) used-gas))
    (set-vm-exec-step! vm (+ (vm-exec-step vm) 1))
  ))

(: simulate-nop! (-> vm-exec Void))
(define (simulate-nop! vm) (void))

(: simulate-push! (-> vm-exec Fixnum Integer))
(define (simulate-push! vm size n) (push-stack! vm n))

(: simulate-asm! (-> vm-exec Symbol vm-exec))
(define (simulate-asm! vm sym)
  (cond ((eq? sym 'ISZERO) (simulate-unop!  vm (λ (a) (if (= a 0) 1 0))))
        ((eq? sym 'ADD)    (simulate-binop! vm (λ (a b) (+ a b))))
        ((eq? sym 'SUB)    (simulate-binop! vm (λ (a b) (- a b))))
        ((eq? sym 'MUL)    (simulate-binop! vm (λ (a b) (* a b))))
        ((eq? sym 'DIV)    (simulate-binop! vm (λ (a b) (if (equal? 0 b) 0 (/ a b)))))
        ((eq? sym 'MOD)    (simulate-binop! vm (λ (a b) (if (equal? 0 b) 0 (modulo a b)))))
        ((eq? sym 'EQ)     (simulate-binop! vm (λ (a b) (if (= a b) 1 0))))
        ((eq? sym 'LT)     (simulate-binop! vm (λ (a b) (if (< a b) 1 0))))
        ((eq? sym 'GT)     (simulate-binop! vm (λ (a b) (if (> a b) 1 0))))
        ((eq? sym 'POP)    (simulate-pop!   vm))
        ((eq? sym 'DUP1)   (simulate-dup!   vm 1))
        ((eq? sym 'DUP2)   (simulate-dup!   vm 2))
        ((eq? sym 'DUP3)   (simulate-dup!   vm 3))
        ((eq? sym 'DUP4)   (simulate-dup!   vm 4))
        ((eq? sym 'DUP5)   (simulate-dup!   vm 5))
        ((eq? sym 'DUP6)   (simulate-dup!   vm 6))
        ((eq? sym 'DUP7)   (simulate-dup!   vm 7))
        ((eq? sym 'DUP8)   (simulate-dup!   vm 8))
        ((eq? sym 'DUP9)   (simulate-dup!   vm 9))
        ((eq? sym 'SWAP1)  (simulate-swap!  vm 1))
        ((eq? sym 'SWAP2)  (simulate-swap!  vm 2))
        ((eq? sym 'SWAP3)  (simulate-swap!  vm 3))
        ((eq? sym 'SWAP4)  (simulate-swap!  vm 4))
        ((eq? sym 'SWAP5)  (simulate-swap!  vm 5))
        ((eq? sym 'SWAP6)  (simulate-swap!  vm 6))
        ((eq? sym 'SWAP7)  (simulate-swap!  vm 7))
        ((eq? sym 'MSTORE) (simulate-mstore! vm))
        ((eq? sym 'MLOAD)  (simulate-mload! vm))
        ((eq? sym 'JUMP)   (simulate-jump! vm))
        ((eq? sym 'JUMPI)  (simulate-jumpi! vm))
        ((eq? sym 'JUMPDEST) (simulate-nop! vm))
        ((eq? sym 'CODECOPY) (simulate-codecopy! vm))
        ((eq? sym 'RETURN) (simulate-return! vm))
        ((eq? sym 'REVERT) (simulate-revert! vm))
        ((eq? sym 'STOP)   (simulate-stop! vm))
        (else
         (error "Unimplemented vm-exec instruction found - simulate-asm:" sym))))

(: simulate-unop! (-> vm-exec (-> Integer Integer) Void))
(define (simulate-unop! vm f)
  (let ((x1 (pop-stack! vm)))
    (push-stack! vm (f x1))))

(: simulate-binop! (-> vm-exec (-> Integer Integer Integer) Void))
(define (simulate-binop! vm f)
  (let* ([x1 (pop-stack! vm)]
         [x2 (pop-stack! vm)])
    (push-stack! vm (f x1 x2))))

(: simulate-pop! (-> vm-exec Void))
(define (simulate-pop! vm) (pop-stack! vm))

(: simulate-dup! (-> vm-exec Fixnum Void))
(define (simulate-dup! vm amount)
  (let ([ x (get-stack vm (- amount 1)) ])
    (push-stack! vm x)))

(: simulate-swap! (-> vm-exec Fixnum Void))
(define (simulate-swap! vm amount)
  (let* ([x1 (get-stack vm 0)]
         [x2 (get-stack vm amount)]
         [new-stack (list-set (list-set (vm-exec-stack vm) 0 x2) amount x1)])
    (set-vm-exec-stack! vm new-stack)))

(: simulate-mstore! (-> vm-exec Void))
(define (simulate-mstore! vm)
  (let* ([addr (pop-stack! vm)]
         [val  (pop-stack! vm)])
    (write-memory-word! vm addr 32 val)))

(: simulate-mload! (-> vm-exec Void))
(define (simulate-mload! vm)
  (let ((addr (pop-stack! vm)))
    (push-stack! vm (read-memory-word vm addr 32))))

; JUMP and JUMPI subtract 1 to ensure they balance the plus 1 every instruction gets.
(: simulate-jump! (-> vm-exec Void))
(define (simulate-jump! vm)
  (let ((addr (- (pop-stack! vm) 0)))
    (assert-landing-pad vm addr)
    (set-vm-exec-pc! vm addr)))

(: simulate-jumpi! (-> vm-exec Void))
(define (simulate-jumpi! vm)
  (let* ((addr (- (pop-stack! vm) 0))
         (pred (pop-stack! vm)))
    (assert-landing-pad vm addr)
    (unless (eq? 0 pred)
      (set-vm-exec-pc! vm addr))))

(: simulate-codecopy! (-> vm-exec void))
(define (simulate-codecopy! vm)
  (let* ([ dest-addr (pop-stack! vm) ]
         [ src-addr  (pop-stack! vm) ]
         [ len       (pop-stack! vm) ]
         [ dest      (vm-exec-memory vm) ]
         [ src       (vm-exec-bytecode vm) ]
         )
    (when (>= (+ len dest-addr) (bytes-length dest))
      (error "simulate-codecopy!: Exceeded available memory" `(,dest-addr ,len) '>= (bytes-length dest)))
             
    (bytes-copy! dest dest-addr
                 src src-addr
                 (+ src-addr len))))

(: simulate-return! (-> vm-exec Void))
(define (simulate-return! vm)
  (let* ([ addr (pop-stack! vm) ]
         [ len  (pop-stack! vm) ]
         [ bs (read-memory vm addr len) ])
    (raise (exn:evm:return "simulate-return!"
                           (current-continuation-marks)
                           vm
                           bs))))

(: simulate-stop! (-> vm-exec Void))
(define (simulate-stop! vm)
  (raise (exn:evm:return "simulate-stop!"
                         (current-continuation-marks)
                         vm
                         (bytes))))

(: simulate-revert! (-> vm-exec Void))
(define (simulate-revert! vm)
  (raise (exn:evm:throw "simulate-revert!"
                        (current-continuation-marks)
                        vm
                        (bytes))))

(: read-memory-word (-> vm-exec Fixnum Fixnum Integer))
(define (read-memory-word vm addr len)
  (check-addr vm addr)
  (bytes->integer (read-memory vm addr len)
                  #f))

(: read-memory (-> vm-exec Fixnum Fixnum Bytes))
(define (read-memory vm addr len)
  (if (>= addr (bytes-length (vm-exec-memory vm)))
      (make-bytes len 0)
      (subbytes (vm-exec-memory vm) addr (+ addr len))
      )
  )

(: write-memory-word! (-> vm-exec Fixnum Fixnum Integer Void))
(define (write-memory-word! vm addr len val)
  (check-addr vm addr)
  (write-memory! vm addr (integer->bytes val len #f)))

(: write-memory! (-> vm-exec Fixnum Bytes Void))
(define (write-memory! vm addr val)
  (touch-memory! vm (+ addr (bytes-length val)))
  (bytes-copy! (vm-exec-memory vm) addr val))

(: check-addr (-> vm-exec Fixnum Fixnum))
(define (check-addr vm addr)
  (if (equal? (modulo addr 32) 0)
      addr
      (raise (exn:evm:misaligned-addr "check-addr: Misaligned address"
                                      (current-continuation-marks)
                                      vm
                                      addr))))

(: push-stack! (-> vm-exec Integer Void))
(define (push-stack! vm val)
  (set-vm-exec-stack! vm (cons val (vm-exec-stack vm))))

(: pop-stack! (-> vm-exec EthWord))
(define (pop-stack! vm)
  (let ((val (car (vm-exec-stack vm))))
    (set-vm-exec-stack! vm (cdr (vm-exec-stack vm)))
    val))

(: touch-memory! (-> vm-exec Fixnum Void))
(define (touch-memory! vm addr)
  (let ([ old (vm-exec-largest-accessed-memory vm) ])
    (set-vm-exec-largest-accessed-memory! vm (max old addr))))

(: get-stack (-> vm-exec Fixnum EthWord))
(define (get-stack vm amount)
  (list-ref (vm-exec-stack vm) amount))

(: make-instruction-dict (-> EthInstructions (Dict EthInstruction)))
(define (make-instruction-dict is)
  (let ((os-table (make-hash))
        (os 0))
    (for ([ i is ])
      (dict-set! os-table os i)
      (set! os (+ os (instruction-size i))))))

(: instruction-gas (-> vm-exec EthInstruction Fixnum))
(define (instruction-gas vm i)
  (define (C_sstore)  (error "C_sstore unimplemented"))
  (define (C_call)    (error "C_call unimplemented"))
  (define (C_suicide) (error "C_suicide unimplemented"))
  (define (is-asm sym) (equal? i (eth-asm sym)))
  (define (is-asms syms) (ormap is-asm syms))
  (cond ((is-asm 'SSTORE) (C_sstore))
        ((is-asm 'EXP)
         (if (eq? 0 (get-stack vm 1))
             G_exp
             (+ G_exp (* G_expbyte (+ 1 (floor (log (get-stack vm 1) 256)))))))
        ((is-asms '(CALLDATACOPY CODECOPY)) (+ G_verylow (* G_copy (ceiling (/ (get-stack vm 2) 32)))))
        ((is-asm 'EXTCODECOPY) (+ G_extcode (* G_copy (ceiling (/ (get-stack vm 3) 32)))))
        ((is-asm 'LOG0) (+ G_log (* G_logdata (get-stack vm 1)) + (* 0 G_logtopic)))
        ((is-asm 'LOG1) (+ G_log (* G_logdata (get-stack vm 1)) + (* 1 G_logtopic)))
        ((is-asm 'LOG2) (+ G_log (* G_logdata (get-stack vm 1)) + (* 2 G_logtopic)))
        ((is-asm 'LOG3) (+ G_log (* G_logdata (get-stack vm 1)) + (* 3 G_logtopic)))
        ((is-asm 'LOG4) (+ G_log (* G_logdata (get-stack vm 1)) + (* 4 G_logtopic)))
        ((is-asms '(CALL CALLCODE DELEGATECALL)) (C_call))
        ((is-asm 'SUICIDE)   (C_suicide))
        ((is-asm 'CREATE)    G_create)
        ((is-asm 'SHA3)      (+ G_sha3 (* G_sha3word (ceiling (/ (get-stack vm 1) 32)))))
        ((is-asm 'JUMPDEST)  G_jumpdest)
        ((is-asm 'SLOAD)     G_sload)
        ((is-asms W_zero)    G_zero)
        ((is-asms W_base)    G_base)
        ((is-asms W_verylow) G_verylow)
        ((is-asms dups)      G_verylow)
        ((is-asms swaps)     G_verylow)
        ((eth-push? i)       G_verylow)
        ((is-asms W_low)     G_low)
        ((is-asms W_mid)     G_mid)
        ((is-asms W_high)    G_high)
        ((is-asms W_extcode) G_extcode)
        ((is-asm 'BALANCE)   G_balance)
        ((is-asm 'BLOCKHASH) G_blockhash)
        ((is-asm 'REVERT)    0)
        (else
         (error "Unknown instruction - instruction-gas:" i))))

        
                     
(: memory-dict (-> vm-exec (HashRef Symbol EthWord)))
(define (memory-dict vm)
  (let ([ ret null ])
    (for ([ i (in-range 0 (vm-exec-largest-accessed-memory vm) 32) ])
      (let ([ row (list i (read-memory-word vm i 32)) ])
        (set! ret (cons row ret))))
    (reverse ret)))

(define (parse-type type bs)
  (cond ((null? bs) (error "parse-pyramid-result: Uninitialized value"))
        ((equal? type "uint256")   (parse-uint256 bs))
        ((equal? type "uint256[]") (parse-array "uint256" bs))
        (else (error "parse-pyramid-result: Unsupported type:" type))))

(define (type-size type)
  (cond ((eq? type "uint256") 32)
        (else (error "type-size: Unsupported type" type))))

(define (parse-uint256 bs) (bytes->integer bs #f #t))
(define (parse-array type bs)
  (let ([ ret null ])
    (for ([ i (in-range 0 (bytes-length bs) 32) ])
      (let ([ bs2 (subbytes bs i (+ i 32)) ])
        (set! ret (cons (parse-uint256 bs2) ret))))
    ret))

(define (infer-type x)
  (cond ((fixnum? x) "uint256")
        ((null? x) "uint256[]")
        ((list? x) (string-append (infer-type (car x))
                                  "[]"))
        (else (error "infer-type: Unknown type" x))))

; TODO: Incorrect if x is a null value that isn't the shared copy of nil.

(define (vm-tag vm x) (read-memory-word vm x 32))

(define (vm-value vm x)
  (cond ((vm-fixnum? vm x)              (vm-fixnum vm x))
        ((vm-symbol? vm x)              (vm-symbol vm x))
        ((vm-compiled-procedure? vm x)  (vm-compiled-procedure vm x))
        ((vm-primitive-procedure? vm x) (vm-primitive-procedure vm x))
        ((vm-pair? vm x)                (vm-pair vm x))
        ((vm-vector? vm x)              (vm-vector vm x))
        ((vm-null? vm x)                null)
        (else (cons 'unboxed x))))

(define (vm-tag-checker tag) (λ (vm x) (equal? tag (vm-tag vm x))))

(define vm-fixnum? (vm-tag-checker TAG-FIXNUM))
(define vm-symbol? (vm-tag-checker TAG-SYMBOL))
(define vm-compiled-procedure? (vm-tag-checker TAG-COMPILED-PROCEDURE))
(define vm-primitive-procedure? (vm-tag-checker TAG-PRIMITIVE-PROCEDURE))
(define vm-pair? (vm-tag-checker TAG-PAIR))
(define vm-vector? (vm-tag-checker TAG-VECTOR))
(define vm-null? (vm-tag-checker TAG-NIL))

(define (vm-fixnum vm x) (read-memory-word vm (+ x #x20) 32))
(define (vm-symbol vm x) (integer->string (read-memory-word vm (+ x #x20) 32)))
(define (vm-procedure vm x)
  (string-append
   "label-"
   (symbol->string (label-name (dict-ref (*reverse-symbol-table*) (read-memory-word vm (+ x #x20) 32) (label 'ERROR))))))
(define vm-compiled-procedure vm-procedure)
(define vm-primitive-procedure vm-procedure)
(define (vm-pair vm x) (cons (read-memory-word vm (+ x #x20) 32)
                             (read-memory-word vm (+ x #x40) 32)))
(define (vm-vector vm x)
  (let ([ addr (read-memory-word vm (+ x #x60))]
        [ len  (read-memory-word vm (+ x #x40))]
        )
    (subbytes (vm-exec-memory vm) addr (+ addr len))))
                                   

(define (vm-list vm x [ max-recursion 10 ])
  (if (<= max-recursion 0)
      null
      (cond ((vm-null? vm x) null)
            ((vm-pair? vm x)
             (let ([ pair (vm-pair vm x) ]
                   [ i    (- max-recursion 1) ]
                   )
               (cons (car pair)
                     (vm-list vm (cdr pair) i))))
            (else null))))

(define (variable-environment vm)
  (let ([ frames (vm-list vm (read-memory-word vm MEM-ENV 32)) ])
    (define (parse-frame frame-ptr)
      (let* ([ frame (vm-pair vm frame-ptr) ]
             [ vars (vm-list vm (car frame) )]
             [ vals (vm-list vm (cdr frame) )]
             [ sz1 (length vars) ]
             [ sz2 (length vals) ]
             [ sz (min sz1 sz2) ]
             )
        (if (> sz 0)
            (map list
                 (map integer->string (take vars sz))
                 (map (λ (x) (vm-value vm x)) (take vals sz))
                 )
            null)))
    (map parse-frame frames)))

(define (assert-landing-pad vm addr)
  (let ([ ethi (instruction-at vm addr)])
    (if (and (eth-asm? ethi)
             (equal? 'JUMPDEST (eth-asm-name ethi)))
        (void)
        (raise (exn:evm:misaligned-jump "assert-landing-pad"
                                        (current-continuation-marks)
                                        vm
                                        addr)))))

(define (on-simulate-debug reverse-symbol-table)
  (λ (vm i reads)
    (fprintf (current-output-port) "~a" (vm-exec-step vm))
    (write-char #\tab)
    (display (label-name (dict-ref reverse-symbol-table (vm-exec-pc vm) (label ""))))
    (write-char #\tab)
    (display (integer->hex (vm-exec-pc vm)))
    (write-char #\tab)
    (write-json (vm-exec-stack vm))
    (write-char #\tab)
    (write-json (memory-dict vm))
    (write-char #\tab)
    (display i)
    (write-char #\tab)
    (write-json (variable-environment vm))
    (newline)
    ))

(: alloc-nonce (-> Fixnum))
(define (alloc-nonce) (tick-counter! *nonce*))

(: make-txn-create (-> bytes vm-txn))
(define (make-txn-create bytecode)
  (vm-txn (alloc-nonce)     ; nonce
          DEFAULT-GAS-PRICE ; gas price
          DEFAULT-GAS-LIMIT ; gas limit
          null              ; to
          0                 ; value
          28                ; v
          0                 ; r
          0                 ; s
          bytecode          ; input
          ))
          
(: make-txn-message (-> Address bytes vm-txn))
(define (make-txn-message to value input)
  (vm-txn (alloc-nonce)     ; nonce
          DEFAULT-GAS-PRICE ; gas price
          DEFAULT-GAS-LIMIT ; gas limit
          to                ; to
          value             ; value
          28                ; v
          0                 ; r
          0                 ; s
          input             ; input
          ))

(: tick-counter! (-> (Parameter Fixnum) Fixnum))
(define (tick-counter! x)
  (let ([ val (x) ])
    (x (+ 1 val))
    val))
       
(: lookup-bytecode (-> simulator Address bytes))
(define (lookup-bytecode sim addr) (dict-ref (vm-world-accounts (simulator-world sim)) addr))

(: install-bytecode! (-> simulator Address! bytes Void))
(define (install-bytecode! sim addr bs) (dict-set! (vm-world-accounts (simulator-world sim)) addr bs))
