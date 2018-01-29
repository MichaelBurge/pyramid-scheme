#lang typed/racket

; TODO: If I ever remove the no-check from the other modules, remove the unsafe below.
(require typed/racket/unsafe)
(unsafe-provide (all-defined-out))

; Types should be in individual modules, but they aren't exported when using typed/racket/no-check
; So I put them here so I can freely no-check the other modules as needed.

(define-type VariableName Symbol)
(define-type VariableNames (Listof VariableName))

(struct pyr-const ([ value : RegisterValue ]) #:transparent)
(struct pyr-variable ([ name : Symbol ]) #:transparent)
(struct pyr-quoted ([ exp : Pyramid ]) #:transparent)
(struct pyr-assign ([ name : VariableName ] [ value : Pyramid ]) #:transparent)
(struct pyr-definition ([ name : VariableName ] [ body : Pyramid ]) #:transparent)
(struct pyr-if ([ predicate : Pyramid ] [ consequent : Pyramid ] [ alternative : Pyramid ]) #:transparent)
(struct pyr-lambda ([ names : VariableNames ] [ body : Pyramid ]) #:transparent)
(struct pyr-begin ([ body : Pyramids ]) #:transparent)
(struct pyr-application ([ operator : Pyramid ] [ operands : Pyramids ]) #:transparent)
(struct pyr-macro-application ([ name : VariableName ] [ operands : Pyramids ]) #:transparent)
(struct pyr-macro-definition ([ name : VariableName ] [ parameters : DottableParameters ] [ body : Racket ]) #:transparent)
(struct pyr-asm-push ([ size : (U 'shrink Byte)] [ value : EthWord]) #:transparent)
(struct pyr-asm-op ([ name : Symbol ]) #:transparent)
(struct pyr-asm-bytes ([ value : Bytes ]) #:transparent)
(struct pyr-asm-cg ([ exp : Any ]) #:transparent)
(define-type pyr-asm-base (U pyr-asm-push pyr-asm-op pyr-asm-bytes pyr-asm-cg label-definition))
(struct pyr-asm ([ insts : (Listof pyr-asm-base)]) #:transparent)
(define-type Pyramid (U pyr-const
                        pyr-variable
                        pyr-quoted
                        pyr-assign
                        pyr-definition
                        pyr-if
                        pyr-lambda
                        pyr-begin
                        pyr-application
                        pyr-macro-application
                        pyr-macro-definition
                        pyr-asm
                        ))

(define-type PyramidQ Any)
(define-type PyramidQs (Listof Any))

(define-type DottableParameters Any)
(define-type Racket Any)
(define-type Rackets (Listof Any))
(define-type Value Any)
(define-type Pyramids (Listof Pyramid))
(define-type Assertion Any)
(define-type PyrMacro Procedure)

(define-type Pass (-> Pyramid Pyramid))

(define-type pyr-consts (Listof pyr-const))
(define-type pyr-variables (Listof pyr-variable))
(define-type pyr-quoteds (Listof pyr-quoted))
(define-type pyr-assigns (Listof pyr-assign))
(define-type pyr-definitions (Listof pyr-definition))
(define-type pyr-ifs (Listof pyr-if))
(define-type pyr-lambdas (Listof pyr-lambda))
(define-type pyr-begins (Listof pyr-begin))
(define-type pyr-applications (Listof pyr-application))
(define-type pyr-macro-applications (Listof pyr-macro-application))
(define-type pyr-macro-definitions (Listof pyr-macro-definition))
(define-type pyr-asms (Listof pyr-asm))

(define-type Linkage (U 'next 'return label))
(define-type Target RegisterName)
(define-type iseq-or-label (U label-definition inst-seq))
(struct inst-seq ([ needs : RegisterNames ] [ modifies : RegisterNames ] [ statements : Instructions ]) #:transparent)

(define-type RegisterNames (Listof RegisterName))
; A DispatchObject is like an object in an OOP language.
; It's a closure that keeps internal state and has named methods that mutate it.
(define-type DispatchObject (-> Symbol Any * Any))
(define-type Stack DispatchObject)
(define-type Machine DispatchObject)
(define-type Register DispatchObject)

(define-type RegisterName Symbol)

(define-type RegisterValue (U Boolean Symbol Integer String (Listof Integer) (Listof Symbol) (Listof String)))
(struct reg ([name : RegisterName]) #:transparent)
(struct const ([ value : RegisterValue ]) #:transparent)
(struct boxed-const ([ value : RegisterValue ]) #:transparent)
(struct label ([ name : Symbol ]) #:transparent)
(struct op ([ name : Symbol] [ args : MExprs ]) #:transparent)
(struct eth-stack () #:transparent)
(define stack (eth-stack))
(define stack? eth-stack?)
(define-type MExpr (U reg
                      const
                      boxed-const
                      op
                      Symbol
                      label
                      eth-stack
                      ))
(define-type MExprs (Listof MExpr))

(struct assign ([ reg-name : RegisterName ] [ value : MExpr ]) #:transparent)
(struct test ([ condition : MExpr ]) #:transparent)
(struct branch ([ dest : MExpr ]) #:transparent)
(struct goto ([ dest : MExpr ]) #:transparent)
(struct save ([ reg-name : RegisterName ]) #:transparent)
(struct restore ([ reg-name : RegisterName ]) #:transparent)
(struct perform ([ action : op ]) #:transparent)
(struct am-asm ([ insts : EthInstructions ]))
(define-type Instruction (U label-definition
                            assign
                            test
                            branch
                            goto
                            save
                            restore
                            perform
                            pyr-asm
                            ))
(define-type Instructions (Listof Instruction))

; Code generator
(struct eth-asm     ([ name : Symbol ]) #:transparent)
(struct eth-push    ([ size : (U 'shrink Byte) ] [ value : (U Integer Symbol label) ]) #:transparent)
(struct eth-unknown ([ byte : Byte ]) #:transparent)
(define-type EthInstruction     (U eth-asm eth-push eth-unknown label-definition))
(define-type EthInstructions    (Listof   EthInstruction))
(define-type Generator0         (->       EthInstructions))
(define-type (Generator  A)     (-> A     EthInstructions))
(define-type (Generator2 A B)   (-> A B   EthInstructions))
(define-type (Generator3 A B C) (-> A B C EthInstructions))

(struct opcode ([ byte : Byte ] [ name : Symbol ] [ num-reads : Fixnum ] [ num-writes : Fixnum ]) #:transparent)

(define-type EthWord Integer)
(define-type EthWords (Listof EthWord))

(define-type SymbolTable (Mutable-HashTable Symbol Integer))
(define-type ReverseSymbolTable (Mutable-HashTable Integer Symbol))
(define-type RelocationTable (Setof relocation))

(struct full-compile-result ([ bytes : Bytes ] [ abstract-insts : Instructions ] [ eth-insts : EthInstructions ]) #:transparent)
(struct relocation ([ pos : Integer ] [ symbol : Symbol ]) #:transparent)
(struct patchpoint ([ symbol : Symbol ] [ initializer : EthInstructions ]) #:transparent)
(struct label-definition label ([ offset : Integer ] [ virtual : Boolean ]) #:transparent)

; A VM execution state

(define-type CodeHash EthWord)
(struct simulator ([ accounts : vm-world ] [ store : vm-store ] [ code-db : (HashTable CodeHash Bytes) ] ) #:transparent)

; Exception types
(struct exn:evm exn:fail ([ vm : vm-exec ]) #:transparent)
(struct exn:evm:return exn:evm ([ result : Bytes ]) #:transparent)
(struct exn:evm:misaligned-addr exn:evm ([ addr : EthWord ]) #:transparent)
(struct exn:evm:throw exn:evm ([ value : Bytes ]) #:transparent)
(struct exn:evm:did-not-halt exn:evm ([ max-iterations : Fixnum ]) #:transparent)
(struct exn:evm:stack-underflow exn:evm ([ ethi : EthInstruction ] [ num-elements : Fixnum ] [ stack : (Listof EthWord )]) #:transparent)
(struct exn:evm:misaligned-jump exn:evm ([ addr : EthWord ]))

(struct simulation-result ([ vm : vm-exec ] [ val : Bytes ] [ txn-receipt : vm-txn-receipt ]) #:transparent)
(define-type simulation-result-ex (U simulation-result exn:evm))
(define-type simulation-result-exs (Listof simulation-result-ex))

(define-type Address Integer) ; Ethereum addresses
(define-type AddressEx (U Null Address))
(define-type StorageRoot EthWord)
(define-type LinkedOffset Integer)
(define-type UnlinkedOffset Integer)

(struct vm-account ([ nonce : Integer ] [ balance : EthWord ] [ storage-root : StorageRoot ] [ code-hash : CodeHash ]) #:mutable)
(struct vm-txn ([ nonce : Integer ]
                [ gas-price : Integer ]
                [ gas-limit : Integer ]
                [ to : (U 'empty Address) ]
                [ value : Integer ]
                [ v : Fixnum ]
                [ r : Integer ]
                [ s : Integer ]
                [ data : Bytes ])
  #:transparent
  #:mutable
  )

(define-type Undefined Any)
(define-type vm-checkpoint Undefined)
(define-type vm-log Undefined)
(define-type vm-world (Mutable-HashTable Address vm-account))

(struct vm-block ([ parent-hash : EthWord ]
                  [ ommers-hash : EthWord ]
                  [ beneficiary : EthWord ]
                  [ state-root : EthWord ]
                  [ transactions-root : EthWord ]
                  [ receipts-root : EthWord ]
                  [ logs-bloom : Undefined ]
                  [ difficulty : EthWord ]
                  [ number : Fixnum ]
                  [ gas-limit : Fixnum ]
                  [ gas-used : Fixnum ]
                  [ timestamp : Fixnum ]
                  [ extra-data : EthWord ]
                  [ mix-hash : EthWord ]
                  [ nonce : Integer ])
  #:transparent
  )

(struct vm-txn-receipt (; EVM spec
                        [ post-transaction : vm-world ]
                        [ cumulative-gas : Integer ]
                        [ log-bloom : Undefined ]
                        [ logs : (Listof vm-log) ]
                        ; Additional fields
                        [ contract-address : AddressEx ]
                        )
  #:transparent
  )
(struct vm-txn-substate ([ suicides : (Setof Address) ] [ log-series : (Setof vm-checkpoint) ] [ refund-balance : Fixnum ]) #:transparent)
(struct vm-exec-environment ([ contract : Address ] ; Account currently executing
                             [ origin : Address ]
                             [ gas-price : EthWord ]
                             [ input-data : Bytes ]
                             [ sender : Address ]
                             [ value : EthWord ]
                             [ bytecode : Bytes ]
                             [ header : vm-block ]
                             [ depth : Fixnum ]
                             [ writable? : Boolean ])
  #:transparent
  )
(struct vm-exec ([ step : Fixnum ]
                 [ pc : Fixnum ]
                 [ stack : (Listof EthWord) ]
                 [ memory : Bytes ]
                 [ gas : Fixnum ]
                 [ largest-accessed-memory : Fixnum ]
                 [ env : vm-exec-environment ]
                 [ sim : simulator ]
                 )
  #:mutable
  )

(define-type account-storage (HashTable EthWord EthWord)) ; An individual account's storage
(define-type world-storage (HashTable Address account-storage)) ; All accounts' storages
(define-type history-storage (HashTable StorageRoot world-storage)) ; All historical commit checkpoints

(define-type AbiType (U "void" "uint256" "uint256[]"))

(struct vm-store ([ history : history-storage ]
                  [ world : world-storage ]
                  [ account : account-storage ])
  #:mutable)

(define-type test-mod (-> test-txn Void))
(define-type test-mods (Listof test-mod))

(struct test-expectation ([ name : String ] [ expected : Any ] [ actual : (-> simulation-result-ex Any)]))
(struct test-txn ([ mods : PyramidQs ] [ tests : (Listof test-expectation) ]) #:transparent #:mutable)
(struct test-account ([ name : Symbol ] [ balance : EthWord ]))
(struct test-case ([ name : String ] [ accounts : test-accounts ] [ deploy-txn : test-txn ][ msg-txns : test-txns]) #:transparent #:mutable)
(struct test-suite ([ name : String ] [ cases : (Listof test-case) ]) #:transparent)

(define-type test-expectations (Listof test-expectation))
(define-type test-txns (Listof test-txn))
(define-type test-accounts (Listof test-account))
(define-type test-cases (Listof test-case))
(define-type Verbosity Fixnum)

(define-type OnSimulateCallback (-> vm-exec EthInstruction EthWords Void))

; Constructors

(: make-symbol-table (-> SymbolTable))
(define (make-symbol-table) (make-hash))

(: make-reverse-symbol-table (-> ReverseSymbolTable))
(define (make-reverse-symbol-table) (make-hash))

(: make-relocation-table (-> RelocationTable))
(define (make-relocation-table) (set))
