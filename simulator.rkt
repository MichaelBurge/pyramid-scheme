#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "disassembler.rkt")
(require "serializer.rkt")
(require "codegen.rkt")
(require "globals.rkt")
(require binaryio/integer)

(provide (all-defined-out))

(define MEMORY-SIZE 30000)

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

(: make-vm (-> Bytes Procedure Procedure evm))
(define (make-vm bytes on-simulate on-return on-error)
  (evm 1     ; step
       bytes ; bytecode
       0     ; pc
       null  ; stack
       (make-bytes MEMORY-SIZE) ; memory
       0     ; gas
       #f    ; halted?
       0     ; largest memory access
       on-simulate
       on-return
       on-error
       ))

(: simulate! (-> evm Fixnum Void))
(define (simulate! vm max-iterations)
  (if (or (<= max-iterations 0)
          (evm-halted? vm))
      '()
      (begin
        (simulate-one! vm (next-instruction vm))
        (simulate! vm (- max-iterations 1)))))

(: next-instruction (-> evm EthInstruction))
(define (next-instruction vm) (cdr (disassemble-one (evm-bytecode vm) (evm-pc vm))))
  
(: simulate-one! (-> evm EthInstruction evm))
(define (simulate-one! vm i)
  (let* ([ used-gas (instruction-gas vm i) ]
         [ op       (eth-to-opcode i) ]
         [ reads (take (evm-stack vm) (opcode-num-reads op)) ]
         )
    ((evm-on-simulate vm) vm i reads)
    (cond ((label? i)    (simulate-nop!  vm))
          ((eth-push? i) (simulate-push! vm (eth-push-size i) (eth-push-value i)))
          ((eth-asm? i)  (simulate-asm!  vm (eth-asm-name i)))
          (else
           (error "Unknown opcode found - simulate-one:" i)))
    (set-evm-pc!   vm (+ (evm-pc   vm) (instruction-size i)))
    (set-evm-gas!  vm (+ (evm-gas  vm) used-gas))
    (set-evm-step! vm (+ (evm-step vm) 1))
  ))

(: simulate-nop! (-> evm Void))
(define (simulate-nop! vm) (void))

(: simulate-push! (-> evm Fixnum Integer))
(define (simulate-push! vm size n) (push-stack! vm n))

(: simulate-asm! (-> evm Symbol evm))
(define (simulate-asm! vm sym)
  (cond ((eq? sym 'ISZERO) (simulate-unop!  vm (λ (a) (if (= a 0) 1 0))))
        ((eq? sym 'ADD)    (simulate-binop! vm (λ (a b) (+ a b))))
        ((eq? sym 'SUB)    (simulate-binop! vm (λ (a b) (- a b))))
        ((eq? sym 'MUL)    (simulate-binop! vm (λ (a b) (* a b))))
        ((eq? sym 'DIV)    (simulate-binop! vm (λ (a b) (/ a b))))
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
         (error "Unimplemented evm instruction found - simulate-asm:" sym))))

(: simulate-unop! (-> evm (-> Integer Integer) Void))
(define (simulate-unop! vm f)
  (let ((x1 (pop-stack! vm)))
    (push-stack! vm (f x1))))

(: simulate-binop! (-> evm (-> Integer Integer Integer) Void))
(define (simulate-binop! vm f)
  (let* ([x1 (pop-stack! vm)]
         [x2 (pop-stack! vm)])
    (push-stack! vm (f x1 x2))))

(: simulate-pop! (-> evm Void))
(define (simulate-pop! vm) (pop-stack! vm))

(: simulate-dup! (-> evm Fixnum Void))
(define (simulate-dup! vm amount)
  (let ([ x (get-stack vm (- amount 1)) ])
    (push-stack! vm x)))

(: simulate-swap! (-> evm Fixnum Void))
(define (simulate-swap! vm amount)
  (let* ([x1 (get-stack vm 0)]
         [x2 (get-stack vm amount)]
         [new-stack (list-set (list-set (evm-stack vm) 0 x2) amount x1)])
    (set-evm-stack! vm new-stack)))

(: simulate-mstore! (-> evm Void))
(define (simulate-mstore! vm)
  (let* ([addr (pop-stack! vm)]
         [val  (pop-stack! vm)])
    (write-memory-word! vm addr 32 val)))

(: simulate-mload! (-> evm Void))
(define (simulate-mload! vm)
  (let ((addr (pop-stack! vm)))
    (push-stack! vm (read-memory-word vm addr 32))))

; JUMP and JUMPI subtract 1 to ensure they balance the plus 1 every instruction gets.
(: simulate-jump! (-> evm Void))
(define (simulate-jump! vm)
  (let ((addr (pop-stack! vm)))
    (set-evm-pc! vm (- addr 1))))

(: simulate-jumpi! (-> evm Void))
(define (simulate-jumpi! vm)
  (let* ((addr (pop-stack! vm))
         (pred (pop-stack! vm)))
    (unless (eq? 0 pred)
      (set-evm-pc! vm (- addr 1)))))

(: simulate-codecopy! (-> evm void))
(define (simulate-codecopy! vm)
  (let* ([ dest-addr (pop-stack! vm) ]
         [ src-addr  (pop-stack! vm) ]
         [ len       (pop-stack! vm) ]
         [ dest      (evm-memory vm) ]
         [ src       (evm-bytecode vm) ]
         )
    (when (>= (+ len dest-addr) (bytes-length dest))
      (error "simulate-codecopy!: Exceeded available memory" `(,dest-addr ,len) '>= (bytes-length dest)))
             
    (bytes-copy! dest dest-addr
                 src src-addr
                 (+ src-addr len))))

(: simulate-return! (-> evm Void))
(define (simulate-return! vm)
  (let* ([ addr (pop-stack! vm) ]
         [ len  (pop-stack! vm) ]
         [ bs (read-memory vm addr len) ])
    ((evm-on-return vm) vm bs)
    (halt! vm)))

(: simulate-stop! (-> evm Void))
(define (simulate-stop! vm)
  ((evm-on-return vm) vm (bytes))
  (halt! vm))

(: simulate-revert! (-> evm Void))
(define (simulate-revert! vm)
  (raise (exn:evm:throw "simulate-revert!"
                        (current-continuation-marks)
                        vm
                        (bytes)))
  (halt! vm))

(: read-memory-word (-> evm Fixnum Fixnum Integer))
(define (read-memory-word vm addr len)
  (check-addr vm addr)
  (bytes->integer (read-memory vm addr len)
                  #f))

(: read-memory (-> evm Fixnum Fixnum Bytes))
(define (read-memory vm addr len)
  (if (>= addr (bytes-length (evm-memory vm)))
      (make-bytes len 0)
      (subbytes (evm-memory vm) addr (+ addr len))
      )
  )

(: write-memory-word! (-> evm Fixnum Fixnum Integer Void))
(define (write-memory-word! vm addr len val)
  (check-addr vm addr)
  (write-memory! vm addr (integer->bytes val len #f)))

(: write-memory! (-> evm Fixnum Bytes Void))
(define (write-memory! vm addr val)
  (touch-memory! vm (+ addr (bytes-length val)))
  (bytes-copy! (evm-memory vm) addr val))

(: check-addr (-> evm Fixnum Fixnum))
(define (check-addr vm addr)
  (if (equal? (modulo addr 32) 0)
      addr
      (raise (exn:evm:misaligned-addr "check-addr: Misaligned address"
                                      (current-continuation-marks)
                                      vm
                                      addr))))

(: push-stack! (-> evm Integer Void))
(define (push-stack! vm val)
  (set-evm-stack! vm (cons val (evm-stack vm))))

(: pop-stack! (-> evm EthWord))
(define (pop-stack! vm)
  (let ((val (car (evm-stack vm))))
    (set-evm-stack! vm (cdr (evm-stack vm)))
    val))

(: touch-memory! (-> evm Fixnum Void))
(define (touch-memory! vm addr)
  (let ([ old (evm-largest-accessed-memory vm) ])
    (set-evm-largest-accessed-memory! vm (max old addr))))

(: halt! (-> evm Void))
(define (halt! vm) (set-evm-halted?! vm true))

(: get-stack (-> evm Fixnum EthWord))
(define (get-stack vm amount)
  (list-ref (evm-stack vm) amount))

(: make-instruction-dict (-> EthInstructions (Dict EthInstruction)))
(define (make-instruction-dict is)
  (let ((os-table (make-hash))
        (os 0))
    (for ([ i is ])
      (dict-set! os-table os i)
      (set! os (+ os (instruction-size i))))))

(: instruction-gas (-> evm EthInstruction Fixnum))
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

        
                     
(: memory-dict (-> evm (HashRef Symbol EthWord)))
(define (memory-dict vm)
  (let ([ ret null ])
    (for ([ i (in-range 0 (evm-largest-accessed-memory vm) 32) ])
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
    (subbytes (evm-memory vm) addr (+ addr len))))
                                   

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
