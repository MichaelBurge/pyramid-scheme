#lang typed/racket

(require (submod "types.rkt" common))
(require (submod "types.rkt" evm-assembly))
(require (submod "types.rkt" simulator))
(require (submod "types.rkt" abstract-machine))
(require "disassembler.rkt")
(require "serializer.rkt")
(require "codegen.rkt")
(require "globals.rkt")
(require "storage.rkt")
(require "crypto.rkt")
(require "io.rkt")
(require "wallet.rkt")
(require "transaction.rkt")
(require "utils.rkt")
(require "expander.rkt")
(require "abstract-analyzer.rkt")
(require typed/racket/unsafe)
(require math)

(unsafe-require/typed "unsafe.rkt"
                     [ unsafe-cast (All (A B) (-> A B))])

(require (submod "typed.rkt" binaryio))
(require (submod "typed.rkt" dict))

(provide
 make-simulator
 apply-txn-create!
 apply-txn-message!
 mint-ether!
 ;
 simres-account-balance
 vm-exec-bytecode
 ;
 read-memory-word
 )

(: make-simulator (-> simulator))
(define (make-simulator) (simulator (make-hash) (make-store) (make-hash)))

(: apply-txn-create! (-> simulator vm-txn simulation-result-ex))
(define (apply-txn-create! sim txn)
  (let ([ vm (make-vm-exec sim txn (vm-txn-data txn)) ])
    (with-handlers ([ exn:evm:return? (λ ([ x : exn:evm:return ])
                                        (define bs (exn:evm:return-result x ))
                                        (define receipt (vm->receipt vm bs))
                                        (define addr (vm-txn-receipt-contract-address receipt ))
                                        (define val (vm-txn-value txn))
                                        (assert addr integer?)
                                        (create-account! sim addr val bs)
                                        (sim-lookup-bytecode sim addr)
                                        (simulation-result vm bs receipt))])
      (simulate! vm (*max-simulation-steps*))
      (error "apply-txn-create!: Reached end of function without explicit termination")
    )))


(: apply-txn-message! (-> simulator vm-txn simulation-result-ex))
(define (apply-txn-message! sim txn)
  (: to Address)
  (define to (cast (vm-txn-to txn) Address))
  (store-set-account! (simulator-store sim) to)
  (let* ([ bytecode (sim-lookup-bytecode sim to)]
         [ vm (make-vm-exec sim txn bytecode)]
         )
    (with-handlers ([ exn:evm:return? (λ ([ x : exn:evm:return ])

                                        (let ([ bs : Bytes (exn:evm:return-result x) ])
                                          (simulation-result vm bs (vm->receipt vm bs))))]
                    [ exn:evm? (λ ([ x : exn:evm ]) x)])
      (transfer-value! vm txn)
      (simulate! vm (*max-simulation-steps*))
      (error "apply-txn-message!: Reached end of function without explicit termination")
      )))

(: transfer-value! (-> vm-exec vm-txn Void))
(define (transfer-value! vm txn)
  (let ([ to    (cast (vm-txn-to txn) Address) ]
        [ from (txn-sender txn)]
        )
    (transfer-money! vm (vm-txn-value txn) from to)
    ))

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

(: vm->receipt (-> vm-exec Bytes vm-txn-receipt))
(define (vm->receipt vm bs)
  (vm-txn-receipt (simulator-accounts (vm-exec-sim vm)) ; post-transaction world
                  (vm-exec-gas vm)                   ; cumulative gas
                  'undefined                         ; log bloom
                  '()                                ; logs list
                  (tick-counter! *contract-create-counter*) ; contract address
                  )
  )

(: make-vm-exec (-> simulator vm-txn Bytes vm-exec))
(define (make-vm-exec sim txn bytecode)
  (: undefined (All (A) (-> A)))
  (define (undefined) (unsafe-cast null))
  (define block (vm-block (undefined) ; parent-hash
                          (undefined) ; ommers-hash
                          (undefined) ; beneficiary
                          (undefined) ; state-root
                          (undefined) ; transactions-root
                          (undefined) ; receipts-root
                          (undefined) ; logs-bloom
                          (undefined) ; difficulty
                          (undefined) ; number
                          (undefined) ; gas-limit
                          (undefined) ; gas-used
                          (undefined) ; timestamp
                          (undefined) ; extra-data
                          (undefined) ; mix-hash
                          (undefined) ; nonce
                          ))
  (: to AddressEx)
  (define to (vm-txn-to txn))
  (define env (vm-exec-environment (if (integer? to) to 0); contract
                                   (undefined)            ; origin
                                   (vm-txn-gas-price txn) ; gas-price
                                   (vm-txn-data txn)      ; input-data
                                   (txn-sender txn)       ; sender
                                   (vm-txn-value txn)     ; value
                                   bytecode               ; bytecode
                                   block                  ; block header
                                   0                      ; depth
                                   #t                     ; writable?
                                   ))
  (vm-exec 1     ; step
           0     ; pc
           null  ; stack
           (make-bytes MEMORY-SIZE) ; memory
           0     ; gas
           0     ; largest memory access
           env
           sim
           ))

(: simulate! (-> vm-exec Natural Void))
(define (simulate! vm max-iterations)
  (for ([ i (in-range max-iterations)])
    (simulate-one! vm (next-instruction vm)))
  (raise (exn:evm:did-not-halt "run-until-return" (current-continuation-marks) vm max-iterations)))

(: instruction-at (-> vm-exec Natural EthInstruction))
(define (instruction-at vm addr)
  (disassemble-one (vm-exec-bytecode vm) addr))

(: next-instruction (-> vm-exec EthInstruction))
(define (next-instruction vm)
  (instruction-at vm (vm-exec-pc vm)))

(: simulate-one! (-> vm-exec EthInstruction Void))
(define (simulate-one! vm i)
  (when (evm-bytes? i)
    (error "Unknown opcode found - simulate-one:" i))
  (let* ([ used-gas (instruction-gas vm i) ]
         [ op       (ethi->opcode i) ]
         [ stk      (vm-exec-stack vm) ]
         [ num-reads (opcode-num-reads op) ]
         [ reads (if (>= (length stk) num-reads)
                     (take stk num-reads)
                     (raise (exn:evm:stack-underflow "simulate-one!" (current-continuation-marks) vm i num-reads stk)))]
         )
    ((*on-simulate-instruction*) vm i reads)
    (match i
      [(struct label-definition _)          (simulate-nop!  vm)]
      [(struct evm-push ((? byte? size)
                         (? exact-nonnegative-integer? val))) (simulate-push! vm size val)]
      [(struct evm-push _)                  (error "simulate-one!: Can only push integers" i)]
      [(struct evm-op (sym))                (simulate-asm!  vm (evm-op-name i))]
      [_                                    (error "Unknown opcode found - simulate-one:" i)]
      )
    (set-vm-exec-pc!   vm (+ (vm-exec-pc   vm) (instruction-size i)))
    (set-vm-exec-gas!  vm (+ (vm-exec-gas  vm) used-gas))
    (set-vm-exec-step! vm (+ (vm-exec-step vm) 1))
  ))

(: simulate-nop! (-> vm-exec Void))
(define (simulate-nop! vm) (void))

(: simulate-push! (-> vm-exec Integer EthWord Void))
(define (simulate-push! vm size n) (push-stack! vm n))

(: simulate-asm! (-> vm-exec Symbol Void))
(define (simulate-asm! vm sym)
  (match sym
    [ 'ISZERO    (simulate-unop!  vm (λ (a) (if (= a 0) 1 0)))]
    [ 'ADD       (simulate-binop! vm (λ (a b) (+ a b)))]
    [ 'SUB       (simulate-binop! vm (λ (a b) (- a b)))]
    [ 'MUL       (simulate-binop! vm (λ (a b) (* a b)))]
    [ 'DIV       (simulate-binop! vm (λ (a b) (if (equal? 0 b) 0 (exact-floor (/ a b)))))]
    [ 'MOD       (simulate-binop! vm (λ (a b) (if (equal? 0 b) 0 (modulo a b))))]
    [ 'EQ        (simulate-binop! vm (λ (a b) (if (= a b) 1 0)))]
    [ 'LT        (simulate-binop! vm (λ (a b) (if (< a b) 1 0)))]
    [ 'GT        (simulate-binop! vm (λ (a b) (if (> a b) 1 0)))]
    [ 'SLT       (simulate-binop! vm (λ (a b) (if (< (word->integer a) (word->integer b)) 1 0)))]
    [ 'SGT       (simulate-binop! vm (λ (a b) (if (> (word->integer a) (word->integer b)) 1 0)))]
    [ 'AND       (simulate-binop! vm (λ (a b) (bitwise-and a b)))]
    [ 'OR        (simulate-binop! vm (λ (a b) (bitwise-ior a b)))]
    [ 'XOR       (simulate-binop! vm (λ (a b) (bitwise-xor a b)))]
    [ 'NOT       (simulate-unop!  vm (λ (a) (truncate-int (bitwise-not a))))]
    [ 'EXP       (simulate-binop! vm (λ (a b) (modular-expt a b WORDLIMIT)))]
    [ 'POP       (simulate-pop!   vm)]
    [ 'DUP1      (simulate-dup!   vm 1)]
    [ 'DUP2      (simulate-dup!   vm 2)]
    [ 'DUP3      (simulate-dup!   vm 3)]
    [ 'DUP4      (simulate-dup!   vm 4)]
    [ 'DUP5      (simulate-dup!   vm 5)]
    [ 'DUP6      (simulate-dup!   vm 6)]
    [ 'DUP7      (simulate-dup!   vm 7)]
    [ 'DUP8      (simulate-dup!   vm 8)]
    [ 'DUP9      (simulate-dup!   vm 9)]
    [ 'SWAP1     (simulate-swap!  vm 1)]
    [ 'SWAP2     (simulate-swap!  vm 2)]
    [ 'SWAP3     (simulate-swap!  vm 3)]
    [ 'SWAP4     (simulate-swap!  vm 4)]
    [ 'SWAP5     (simulate-swap!  vm 5)]
    [ 'SWAP6     (simulate-swap!  vm 6)]
    [ 'SWAP7     (simulate-swap!  vm 7)]
    [ 'MSTORE    (simulate-mstore! vm)]
    [ 'MLOAD     (simulate-mload! vm)]
    [ 'SLOAD     (simulate-sload! vm)]
    [ 'SSTORE    (simulate-sstore! vm)]
    [ 'JUMP      (simulate-jump! vm)]
    [ 'JUMPI     (simulate-jumpi! vm)]
    [ 'JUMPDEST  (simulate-nop! vm)]
    [ 'CODECOPY  (simulate-codecopy! vm)]
    [ 'RETURN    (simulate-return! vm)]
    [ 'REVERT    (simulate-revert! vm)]
    [ 'STOP      (simulate-stop! vm)]
    [ 'ADDRESS   (simulate-env! vm vm-exec-environment-contract)]
    [ 'CALLER    (simulate-env! vm vm-exec-environment-sender)]
    [ 'CALLVALUE (simulate-env! vm vm-exec-environment-value)]
    [ 'CALLDATASIZE (simulate-env! vm (compose bytes-length vm-exec-environment-input-data))]
    [ 'CALLDATALOAD (simulate-calldataload! vm)]
    [ 'BALANCE   (simulate-balance! vm)]
    [ 'CALL      (simulate-call! vm)]
    [ 'LOG0      (simulate-log vm 0)]
    [ 'LOG1      (simulate-log vm 1)]
    [ 'LOG2      (simulate-log vm 2)]
    [ 'LOG3      (simulate-log vm 3)]
    [ 'LOG4      (simulate-log vm 4)]
    [ _          (error "simulate-asm - Unimplemented vm-exec instruction found:" sym)]
    ))

(: simulate-unop! (-> vm-exec (-> EthWord EthWord) Void))
(define (simulate-unop! vm f)
  (let ((x1 (pop-stack! vm)))
    (push-stack! vm (assert-natural (f x1)))))

(: simulate-binop! (-> vm-exec (-> EthWord EthWord Integer) Void))
(define (simulate-binop! vm f)
  (let* ([x1 (pop-stack! vm)]
         [x2 (pop-stack! vm)])
    (push-stack! vm (truncate-int (f x1 x2)))))

(: simulate-pop! (-> vm-exec Void))
(define (simulate-pop! vm) (void (pop-stack! vm)))

(: simulate-dup! (-> vm-exec Positive-Integer Void))
(define (simulate-dup! vm amount)
  (let ([ x (get-stack vm (- amount 1)) ])
    (push-stack! vm x)))

(: simulate-swap! (-> vm-exec Natural Void))
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

(: simulate-sload! (-> vm-exec Void))
(define (simulate-sload! vm)
  (let* ([ addr (pop-stack! vm) ]
         [ val (read-storage vm addr) ])
    (push-stack! vm val)))

(: simulate-sstore! (-> vm-exec Void))
(define (simulate-sstore! vm)
  (let* ([ addr (pop-stack! vm) ]
         [ val  (pop-stack! vm) ])
    (write-storage! vm addr val)))

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

(: simulate-codecopy! (-> vm-exec Void))
(define (simulate-codecopy! vm)
  (let* ([ dest-addr (pop-stack! vm) ]
         [ src-addr  (pop-stack! vm) ]
         [ len       (pop-stack! vm) ]
         [ dest      (vm-exec-memory vm) ]
         [ src       (vm-exec-bytecode vm) ]
         )
    (when (>= (+ len dest-addr) (bytes-length dest))
      (error "simulate-codecopy!: Exceeded available memory" `(,dest-addr ,len) '>= (bytes-length dest)))
    (touch-memory! vm (+ dest-addr len))
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

(: simulate-env! (-> vm-exec (-> vm-exec-environment EthWord) Void))
(define (simulate-env! vm f)
  (define env (vm-exec-env vm))
  (push-stack! vm (f env)))

(: simulate-balance! (-> vm-exec Void))
(define (simulate-balance! vm)
  (define addr (pop-stack! vm))
  (define bal (account-balance (vm-exec-sim vm) addr))
  (push-stack! vm bal))

(: simulate-call! (-> vm-exec Void))
(define (simulate-call! vm)
  (let* ([ gas    (pop-stack! vm) ]
         [ to     (pop-stack! vm) ]
         [ val    (pop-stack! vm) ]
         [ in-os  (pop-stack! vm) ]
         [ in-sz  (pop-stack! vm) ]
         [ out-os (pop-stack! vm) ]
         [ out-sz (pop-stack! vm) ]
         [ from   (vm-exec-contract vm)]
         [ to-acc (account vm to) ]
         )
    (unless (equal? 0 (bytes-length (lookup-bytecode vm to)))
      (error "simulate-call!: CALL to nonempty bytecode unsupported"))
    (transfer-money! vm val from to) ; TODO: Change this to apply-txn-message!
    (push-stack! vm 1) ; TODO: CALL shouldn't always succeed.
    ))

(: simulate-calldataload! (-> vm-exec Void))
(define (simulate-calldataload! vm)
  (let* ([ i (pop-stack! vm)]
         [ env (vm-exec-env vm)]
         [ bs (vm-exec-environment-input-data env)]
         [ val (bytes-or-zero bs i 32)])
    (push-stack! vm val)))

(: simulate-log (-> vm-exec Natural Void))
(define (simulate-log vm num-tags)
  (let* ([ ptr (pop-stack! vm) ]
         [ len (pop-stack! vm) ]
         [ ts  (for/list : EthWords ([ i (in-range num-tags) ])
                 (pop-stack! vm))]
         [ bs  (read-memory vm ptr len)])
    ((*on-log*) vm bs ts)))

(: read-memory-word (-> vm-exec EthWord EthWord EthWord))
(define (read-memory-word vm addr len)
  (bytes->nonnegative (read-memory vm addr len)))

(: read-memory (-> vm-exec EthWord EthWord Bytes))
(define (read-memory vm addr len)
  (define largest (vm-exec-largest-accessed-memory vm))
  (if (>= addr largest)
      (begin
        (maybe-warn "read-memory: Out-of-bounds memory read" `(,largest < (+ ,addr ,len)))
        (make-bytes len 0))
      (subbytes (vm-exec-memory vm) addr (+ addr len))
      )
  )

(: write-memory-word! (-> vm-exec EthWord EthWord EthWord Void))
(define (write-memory-word! vm addr len val)
  (write-memory! vm addr (integer->bytes val len #f)))

(: write-memory! (-> vm-exec EthWord Bytes Void))
(define (write-memory! vm addr val)
  (touch-memory! vm (+ addr (bytes-length val)))
  (bytes-copy! (vm-exec-memory vm) addr val))

(: read-storage (-> vm-exec EthWord EthWord))
(define (read-storage vm addr)
  (store-get-value (vm-get-store vm) addr))

(: write-storage! (-> vm-exec EthWord EthWord Void))
(define (write-storage! vm addr val)
  (store-set-value! (vm-get-store vm) addr val))

(: vm-get-store (-> vm-exec vm-store))
(define (vm-get-store vm)
  (let* ([ sim (vm-exec-sim vm) ]
         [ store (simulator-store sim) ])
    store))

(: check-addr (-> vm-exec EthWord EthWord))
(define (check-addr vm addr)
  (if (equal? (modulo addr 32) 0)
      addr
      (raise (exn:evm:misaligned-addr "check-addr: Misaligned address"
                                      (current-continuation-marks)
                                      vm
                                      addr))))

(: push-stack! (-> vm-exec EthWord Void))
(define (push-stack! vm val)
  (set-vm-exec-stack! vm (cons val (vm-exec-stack vm))))

(: pop-stack! (-> vm-exec EthWord))
(define (pop-stack! vm)
  (let ((val (car (vm-exec-stack vm))))
    (set-vm-exec-stack! vm (cdr (vm-exec-stack vm)))
    val))

(: touch-memory! (-> vm-exec EthWord Void))
(define (touch-memory! vm addr)
  (let ([ old (vm-exec-largest-accessed-memory vm) ])
    (set-vm-exec-largest-accessed-memory! vm (max old addr))))

(: get-stack (-> vm-exec Natural EthWord))
(define (get-stack vm amount)
  (list-ref (vm-exec-stack vm) amount))

;; (: make-instruction-dict (-> EthInstructions (HashTable UnlinkedOffset EthInstruction)))
;; (define (make-instruction-dict is)
;;   (let ((os-table (make-hash))
;;         (os 0))
;;     (for ([ i is ])
;;       (hash-set! os-table os i)
;;       (set! os (+ os (instruction-size i))))))

(: empty-hash Bytes)
(define empty-hash (hex-string->bytes "C5D2460186F7233C927E7DB2DCC703C0E500B653CA82273B7BFAD8045D85A470"))

(: empty-hash-word EthWord)
(define empty-hash-word (bytes->nonnegative empty-hash))

(: empty? (-> vm-exec Address Boolean))
(define (empty? vm addr)
  (let ([ acc (account vm addr) ])
    (and
     acc
     (equal? (vm-account-code-hash acc) empty-hash) ; No code
     (= (vm-account-nonce acc) 0)                   ; Zero nonce
     (= (vm-account-balance acc) 0)                 ; Zero balance
     )))

(: dead? (-> vm-exec Address Boolean))
(define (dead? vm addr)
  (or (not (account vm addr))
      (empty? vm addr)))

(: L (-> Integer EthWord))
(define (L n) (assert-natural (- n (exact-floor (/ n 64)))))

(: instruction-gas (-> vm-exec EthInstruction Natural))
(define (instruction-gas vm i)
  (: C_sstore (-> Natural))
  (define (C_sstore)
    (let ([ addr ( get-stack vm 0) ]
          [ val  ( get-stack vm 1) ])
      (if (and (= (read-storage vm addr) 0) (> val 0))
          G_sset
          G_sreset)))
  (: C_call (-> Natural))
  (define (C_call)
    (let* ([ gas       (get-stack vm 0) ]
           [ addr      (get-stack vm 1) ]
           [ val       (get-stack vm 2) ]
           [ val?      (> val 0) ]
           [ u_g       (vm-exec-gas vm)]
           [ C_xfer    (if val? G_callvalue 0)]
           [ C_new     (if (and (dead? vm addr) val?) G_newaccount 0)]
           [ C_extra   (+ G_call C_xfer C_new) ]
           [ C_gascap (if (>= u_g C_extra)
                           (min gas (L (- u_g C_extra)))
                           gas)]
           [ C_callgas (+ C_gascap (if val? G_callstipend 0))]
           )
      (+ C_gascap C_extra)))
  (define (C_suicide) (error "C_suicide unimplemented"))
  (: is-asm (-> Symbol Boolean))
  (define (is-asm sym) (equal? i (evm-op sym)))
  (: is-asms (-> Symbols Boolean))
  (define (is-asms syms) (ormap is-asm syms))
  (cond ((is-asm 'SSTORE) (C_sstore))
        ((is-asm 'EXP)
         (if (eq? 0 (get-stack vm 1))
             G_exp
             (+ G_exp (* G_expbyte (integer-bytes (get-stack vm 1))))))
        ((is-asms '(CALLDATACOPY CODECOPY)) (+ G_verylow (* G_copy (ceiling (/ (get-stack vm 2) 32)))))
        ((is-asm 'EXTCODECOPY) (+ G_extcode (* G_copy (ceiling (/ (get-stack vm 3) 32)))))
        ((is-asm 'LOG0) (+ G_log (* G_logdata (get-stack vm 1)) (* 0 G_logtopic)))
        ((is-asm 'LOG1) (+ G_log (* G_logdata (get-stack vm 1)) (* 1 G_logtopic)))
        ((is-asm 'LOG2) (+ G_log (* G_logdata (get-stack vm 1)) (* 2 G_logtopic)))
        ((is-asm 'LOG3) (+ G_log (* G_logdata (get-stack vm 1)) (* 3 G_logtopic)))
        ((is-asm 'LOG4) (+ G_log (* G_logdata (get-stack vm 1)) (* 4 G_logtopic)))
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
        ((evm-push? i)       G_verylow)
        ((is-asms W_low)     G_low)
        ((is-asms W_mid)     G_mid)
        ((is-asms W_high)    G_high)
        ((is-asms W_extcode) G_extcode)
        ((is-asm 'BALANCE)   G_balance)
        ((is-asm 'BLOCKHASH) G_blockhash)
        ((is-asm 'REVERT)    0)
        (else
         (error "Unknown instruction - instruction-gas:" i))))

(: assert-landing-pad (-> vm-exec Address Void))
(define (assert-landing-pad vm addr)
  (let ([ ethi (instruction-at vm addr)])
    (if (and (evm-op? ethi)
             (equal? 'JUMPDEST (evm-op-name ethi)))
        (void)
        (raise (exn:evm:misaligned-jump "assert-landing-pad"
                                        (current-continuation-marks)
                                        vm
                                        addr)))))

(: sim-lookup-bytecode (-> simulator Address Bytes))
(define (sim-lookup-bytecode sim addr)
  (let* ([ acc (sim-account sim addr) ])
    (if acc
        (hash-ref (simulator-code-db sim)
                  (vm-account-code-hash acc)
                  (λ () (bytes)))
        (bytes))))

(: lookup-bytecode (-> vm-exec Address Bytes))
(define (lookup-bytecode vm addr)
  (sim-lookup-bytecode (vm-exec-sim vm) addr))

(: sim-account (-> simulator Address (U #f vm-account)))
(define (sim-account sim addr)
  (let ([ accs (simulator-accounts sim) ])
    (hash-ref accs addr #f)))

(: account (-> vm-exec Address (U #f vm-account)))
(define (account vm addr)
  (let ([ sim (vm-exec-sim vm) ])
    (sim-account sim addr)))

(: find-or-create-account! (-> simulator Address vm-account))
(define (find-or-create-account! sim addr)
  (: acc (U #f vm-account))
  (define acc (sim-account sim addr))
  (if acc acc (create-account! sim addr 0 (bytes)))
  )

; TODO: Implement by mining a block
(: mint-ether! (-> simulator Address EthWord Void))
(define (mint-ether! sim addr amount)
  (: acc vm-account)
  (define acc (find-or-create-account! sim addr))
  (set-vm-account-balance! acc (+ amount (vm-account-balance acc))))

(: account-balance (-> simulator Address EthWord))
(define (account-balance sim addr)
  (let* ([ accs : vm-world (simulator-accounts sim)]
         [ acc : (U #f vm-account) (hash-ref accs addr #f)])
    (if acc
        (vm-account-balance acc)
        0)))

(: make-code-hash (-> Bytes CodeHash))
(define (make-code-hash bs)
  (if (equal? 0 (bytes-length bs))
      empty-hash-word
      (keccak-256-word bs)))

(: create-account! (-> simulator Address EthWord Bytes vm-account))
(define (create-account! sim addr val bs)
  (define hash (make-code-hash bs))
  (define store-root (store-get-root (simulator-store sim)))
  (define account (vm-account (tick-counter! *account-nonce*) ; nonce
                              val                             ; initial balance
                              store-root                      ; storage root
                              hash                            ; code hash
                              ))
  (hash-set! (simulator-accounts sim) addr account)
  (hash-set! (simulator-code-db sim) hash bs)
  account
  )

(: account-value (-> simulator Address EthWord))
(define (account-value sim addr)
  (vm-account-balance (hash-ref (simulator-accounts sim) addr)))

(: transfer-money! (-> vm-exec EthWord Address Address Void))
(define (transfer-money! vm amount from to)
  (let* ([ from-acc (account vm from) ]
         [ to-acc   (account vm to) ]
         [ from-bal (if from-acc (vm-account-balance from-acc) 0)]
         [ to-bal   (if to-acc   (vm-account-balance to-acc)   0)]
         [ sim      (vm-exec-sim vm)]
         )
    (match* (amount (- from-bal amount))
      [(0 _) (void)]
      [(_ (? negative? x)) (error "transfer-money!: Insufficient balance" (find-addr-name from))]
      [((? positive-integer? amount) (? exact-nonnegative-integer? new-from-bal))
       (unless from-acc
         (print-account-balances vm)
         (error "transfer-money!: Invalid sender" from amount (*addresses-by-name*)))
       (set-vm-account-balance! from-acc new-from-bal)
       (if to-acc
           (set-vm-account-balance! to-acc   (+ to-bal   amount))
           (void (create-account! sim to amount (bytes))))])))

(: vm-exec-contract (-> vm-exec Address))
(define (vm-exec-contract vm)
  (vm-exec-environment-contract (vm-exec-env vm)))

(: maybe-warn (-> String Any * Any))
(define (maybe-warn msg . xs)
  (when (*warnings?*)
    (error msg xs)))

(: simres-account-balance (-> simulation-result Address EthWord))
(define (simres-account-balance simres addr)
  (let* ([ receipt (simulation-result-txn-receipt simres) ]
         [ world (vm-txn-receipt-post-transaction receipt) ]
         [ acc : (U vm-account #f) (hash-ref world addr #f)]
         )
    (if acc
        (vm-account-balance acc)
        0)))

(: vm-exec-bytecode (-> vm-exec Bytes))
(define (vm-exec-bytecode vm) (vm-exec-environment-bytecode (vm-exec-env vm)))
