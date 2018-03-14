#lang typed/racket

(provide (all-defined-out))

;; (require typed/racket/unsafe)
;; (unsafe-provide (all-defined-out))

; These submodules should be in the original file, but this issue prevents that:
; https://github.com/racket/racket/issues/1955
; https://github.com/racket/racket/issues/1101

(module common typed/racket
  (require typed/racket/unsafe)
  (provide (all-defined-out)
           register-value?)

  (define-type Anys (Listof Any))
  (define-type Symbols (Listof Symbol))
  (define-type EthWord Integer)
  (define-type EthWords (Listof EthWord))
  (struct label ([ name : Symbol ]) #:transparent)
  (struct label-definition label ([ offset : Integer ] [ virtual? : Boolean ]) #:transparent)
  (define-type RegisterValue (U Boolean Symbol Integer String (Listof Integer) (Listof Symbol) (Listof String) (Vectorof Integer)))
  (module unsafe racket
    (provide register-value?)
    (define (register-value? x)
      (or (boolean? x)
          (symbol? x)
          (integer? x)
          (string? x)
          (list? x)
          (vector? x)))
    )
  (unsafe-require/typed 'unsafe
    [ register-value? (-> Any Boolean : RegisterValue) ])
  (define-type Verbosity Fixnum)
  (define-type SourceMap (HashTable Symbol Symbols))
  (: make-source-map (-> SourceMap))
  (define (make-source-map) (make-hash))
  )

; codegen.rkt
(module evm-assembly typed/racket
  (require (submod ".." common))
  (provide (all-defined-out))

  (struct opcode ([ byte : Byte ] [ name : Symbol ] [ num-reads : Integer ] [ num-writes : Integer ]) #:transparent)
  (struct evm-op      ([ name : Symbol ]) #:transparent)
  (struct evm-push    ([ size : (U 'shrink Byte) ] [ value : (U Integer Symbol label) ]) #:transparent)
  (struct evm-bytes   ([ bytes : Bytes ]) #:transparent)
  (define-type EthInstruction     (U evm-op evm-push evm-bytes label-definition))
  (define-type EthInstructions    (Listof   EthInstruction))
  (define-type Generator0         (->       EthInstructions))
  (define-type (Generator  A)     (-> A     EthInstructions))
  (define-type Generator1 Generator)
  (define-type (Generator2 A B)   (-> A B   EthInstructions))
  (define-type (Generator3 A B C) (-> A B C EthInstructions))

  (struct relocation ([ pos : Integer ] [ symbol : Symbol ]) #:transparent)
  (struct patchpoint ([ symbol : Symbol ] [ initializer : EthInstructions ]) #:transparent)
  (define-type SymbolTable (Mutable-HashTable Symbol Integer))
  (define-type ReverseSymbolTable (Mutable-HashTable Integer Symbol))
  (define-type RelocationTable (Setof relocation))
  (define-type LinkedOffset Integer)
  (define-type UnlinkedOffset Integer)
  (define-type SourceMapper (-> UnlinkedOffset Symbols))

  (: make-symbol-table (-> SymbolTable))
  (define (make-symbol-table) (make-hash))

  (: make-reverse-symbol-table (-> ReverseSymbolTable))
  (define (make-reverse-symbol-table) (make-hash))

  (: make-relocation-table (-> RelocationTable))
  (define (make-relocation-table) (set))
  )

; compiler.rkt
(module abstract-machine typed/racket
  (require typed/racket/unsafe)
  (require (submod ".." common))
  (require (submod ".." evm-assembly))
  ;(require (submod ".." ast))

  (provide (all-defined-out))

  (define-type RegisterName (U 'env 'proc 'continue 'argl 'val 'stack-size))

  (struct primop ([ name : Symbol ] [ gen : Procedure ] [ eval : Procedure ]) #:transparent)
  (define-type PrimopTable (HashTable Symbol primop))

  (struct assign ([ reg-name : RegisterName ] [ value : MExpr ]) #:transparent)
  (struct test ([ condition : MExpr ]) #:transparent)
  (struct branch ([ dest : MExpr ]) #:transparent)
  (struct goto ([ dest : MExpr ]) #:transparent)
  (struct save ([ exp : MExpr ]) #:transparent)
  (struct restore ([ reg-name : RegisterName ]) #:transparent)
  (struct perform ([ action : MExpr ]) #:transparent)
  (struct evm ([ insts : EthInstructions ]) #:transparent)
  (define-type Instruction (U label-definition
                              assign
                              test
                              branch
                              goto
                              save
                              restore
                              perform
                              evm
                              ))

  (struct reg ([name : RegisterName]) #:transparent)
  (struct const ([ value : RegisterValue ]) #:transparent)
  (struct boxed-const ([ value : RegisterValue ]) #:transparent)
  (struct op ([ name : Symbol] [ args : MExprs ]) #:transparent)
  (struct %stack ())
  (define stack (%stack))
  (define stack? %stack?)
  (define-type MExpr (U reg
                        const
                        boxed-const
                        op
                        Symbol
                        label
                        %stack
                        evm
                        ))

  (struct inst-seq ([ needs : RegisterNames ] [ modifies : RegisterNames ] [ statements : Instructions ]) #:transparent)
  (define-type MExprs (Listof MExpr))
  (define-type Instructions (Listof Instruction))
  (define-type RegisterNames (Setof RegisterName))
  (struct v-fixnum              ([value : Integer] [ ptr : Integer ]) #:mutable #:transparent)
  (struct v-symbol              ([value : Symbol ])        #:transparent)
  (struct v-compiled-procedure  ([label : label  ] [ env : v-environment])  #:transparent)
  (struct v-primitive-procedure ([label : label  ])        #:transparent)
  (struct v-pair                ([left  : value] [right : value])           #:transparent)
  (struct v-vector              ([elems : (Vectorof value)])         #:transparent)
  (struct v-null                (                )         #:transparent)
  (struct v-continuation        ([continue : label ] [env : v-environment] [ stack : values ]) #:transparent)
  (struct v-frame               ([mappings : (HashTable Symbol value)])     #:transparent)
  (struct v-environment         ([frames : v-frames]))
  (define-type v-unboxed (U Integer Symbol Boolean))
  (define-type v-callable (U v-compiled-procedure v-primitive-procedure v-continuation))
  (define-type v-list (U v-null v-pair))
  (define-predicate v-unboxed?  v-unboxed)
  (define-predicate v-callable? v-callable)
  (define-predicate v-list?     v-list)

  (define-type value (U Void
                        v-unboxed
                        v-fixnum
                        v-symbol
                        v-compiled-procedure
                        v-primitive-procedure
                        v-pair
                        v-vector
                        v-null
                        v-continuation
                        v-frame
                        v-environment
                        label))
  (define-type v-frames (Listof v-frame))
  (define-type values (Listof value))
  (struct machine ([pc : Integer]
                   [env : v-environment]
                   [proc : v-callable]
                   [continue : label]
                   [argl : value]
                   [val : value]
                   [stack-size : Integer]
                   [stack : values]
                   [halted? : Boolean])
    #:transparent #:mutable)
  )

; ast.rkt
(module ast typed/racket
  (require (submod ".." common))
  (require (submod ".." abstract-machine))
  (provide (all-defined-out)
           (all-from-out (submod ".." common)))

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
  ;; (struct pyr-asm-push ([ size : (U 'shrink Byte)] [ value : Integer]) #:transparent)
  ;; (struct pyr-asm-op ([ name : Symbol ]) #:transparent)
  ;; (struct pyr-asm-bytes ([ value : Bytes ]) #:transparent)
  ;; (struct pyr-asm-cg ([ exp : Any ]) #:transparent)
  ;(define-type pyr-asm-base (U pyr-asm-push pyr-asm-op pyr-asm-bytes pyr-asm-cg label-definition))
  (struct pyr-asm ([ insts : Instructions]) #:transparent)
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
  (define-type VariableName Symbol)
  (define-type VariableNames (Listof VariableName))
  (define-type PyramidQ (Sexpof (U Any PyramidQ)))
  (define-type PyramidQs (Listof PyramidQ))
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
  )

; simulator.rkt
(module simulator typed/racket
  (require (submod ".." common))
  (require (submod ".." evm-assembly))
  (provide (all-defined-out))

  (define-type CodeHash EthWord)
  (struct simulator ([ accounts : vm-world ] [ store : vm-store ] [ code-db : (HashTable CodeHash Bytes) ] ) #:transparent)

  (define-type Frame (List (Listof String) Anys))
  (define-type Environment (Listof Frame))

  (define-type Frames (Listof Frame))

  (struct exn:evm exn:fail ([ vm : vm-exec ]) #:transparent)
  (struct exn:evm:return exn:evm ([ result : Bytes ]) #:transparent)
  (struct exn:evm:misaligned-addr exn:evm ([ addr : EthWord ]) #:transparent)
  (struct exn:evm:throw exn:evm ([ value : Bytes ]) #:transparent)
  (struct exn:evm:did-not-halt exn:evm ([ max-iterations : Integer ]) #:transparent)
  (struct exn:evm:stack-underflow exn:evm ([ ethi : EthInstruction ] [ num-elements : Integer ] [ stack : (Listof EthWord )]) #:transparent)
  (struct exn:evm:misaligned-jump exn:evm ([ addr : EthWord ]) #:transparent)

  (struct simulation-result ([ vm : vm-exec ] [ val : Bytes ] [ txn-receipt : vm-txn-receipt ]) #:transparent)
  (define-type simulation-result-ex (U simulation-result exn:evm))
  (define-type simulation-result-exs (Listof simulation-result-ex))

  (define-type Address Integer) ; Ethereum addresses
  (define-type AddressEx (U #f Address))
  (define-type StorageRoot EthWord)
  (define-type OnSimulateCallback (-> vm-exec EthInstruction EthWords Void))

  (struct vm-account ([ nonce : Integer ] [ balance : EthWord ] [ storage-root : StorageRoot ] [ code-hash : CodeHash ]) #:mutable #:transparent)
  (struct vm-txn ([ nonce : Integer ]
                  [ gas-price : Integer ]
                  [ gas-limit : Integer ]
                  [ to : AddressEx ]
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
                               [ depth : Integer ]
                               [ writable? : Boolean ])
    #:transparent
    )
  (struct vm-exec ([ step : Integer ]
                   [ pc : Integer ]
                   [ stack : (Listof EthWord) ]
                   [ memory : Bytes ]
                   [ gas : Integer ]
                   [ largest-accessed-memory : Integer ]
                   [ env : vm-exec-environment ]
                   [ sim : simulator ]
                   )
    #:mutable
    )

  (define-type account-storage (HashTable EthWord EthWord)) ; An individual account's storage
  (define-type world-storage (HashTable Address account-storage)) ; All accounts' storages
  (define-type history-storage (HashTable StorageRoot world-storage)) ; All historical commit checkpoints

  (define-type AbiType (U "void" "uint256" "uint256[]" "bool"))

  (struct vm-store ([ history : history-storage ]
                    [ world : world-storage ]
                    [ account : account-storage ])
    #:mutable)
)

; test.rkt
(module test typed/racket
  (require (submod ".." common))
  (require (submod ".." ast))
  (require (submod ".." simulator))
  (provide (all-defined-out))

  (define-type test-mod (-> test-txn Void))
  (define-type test-mods (Listof test-mod))

  (struct test-expectation ([ name : String ] [ expected : Any ] [ actual : (-> simulation-result-ex Any)]) #:transparent)
  (struct test-txn ([ mods : PyramidQs ] [ tests : (Listof test-expectation) ]) #:transparent #:mutable)
  (struct test-account ([ name : Symbol ] [ balance : EthWord ]) #:transparent)
  (struct test-case ([ name : String ] [ accounts : test-accounts ] [ deploy-txn : test-txn ][ msg-txns : test-txns]) #:transparent #:mutable)
  (struct test-suite ([ name : String ] [ cases : (Listof test-case) ]) #:transparent)

  (define-type test-expectations (Listof test-expectation))
  (define-type test-txns (Listof test-txn))
  (define-type test-accounts (Listof test-account))
  (define-type test-cases (Listof test-case))
  )
