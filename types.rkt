#lang typed/racket

; TODO: If I ever remove the no-check from the other modules, remove the unsafe below.
(require typed/racket/unsafe)
(unsafe-provide (all-defined-out))

; Types should be in individual modules, but they aren't exported when using typed/racket/no-check
; So I put them here so I can freely no-check the other modules as needed.

(define-type VariableName Symbol)
(define-type VariableNames (Listof VariableName))

(define-type PyrSelfEvaluating (U Number String))
(define-type PyrVariable          Symbol)
(define-type PyrQuote      (List 'quote  Pyramid))
(define-type PyrAssign     (List 'set!   Target Pyramid))
(define-type PyrDefinition (List 'define (U VariableName VariableNames) Pyramid))
(define-type PyrIf         (List 'if     Pyramid Pyramid (U Nothing Pyramid)))
(define-type PyrLambda     (List 'lambda VariableNames Sequence))
(define-type PyrBegin      (List 'begin  (Listof Pyramid)))
(define-type PyrApplication (Pairof Pyramid Sequence))
(define-type PyrMacro      (List 'defmacro (U VariableName VariableNames) (Listof Racket)))
(define-type PyrAsmPush    (List 'push (U 'shrink Fixnum) Fixnum))
(define-type PyrAsmOp      (List 'op Symbol))
(define-type PyrAsmByte    (List 'byte Fixnum))
(define-type PyrAsmLabel   (List 'label Symbol))
(define-type PyrAsm        (List 'asm (Listof (U PyrAsmPush PyrAsmOp PyrAsmByte PyrAsmLabel))))
(define-type Pyramid (U PyrSelfEvaluating
                        PyrVariable
                        PyrQuote
                        PyrAssign
                        PyrDefinition
                        PyrIf
                        PyrLambda
                        PyrBegin
                        PyrApplication
                        PyrMacro
                        PyrAsm
                        ))

(define-type Racket Any)
(define-type Value Any)
(define-type Sequence (Listof Pyramid))
(define-type Assertion Any)

(define-type Linkage (U 'next 'return LabelName))
(define-type Target RegisterName)
(define-type iseq-or-label (U label inst-seq))
(struct inst-seq ([ needs : RegisterNames ] [ modifies : RegisterNames ] [ statements : Instructions ]) #:transparent)

(define-type RegisterNames (Listof RegisterName))
; A DispatchObject is like an object in an OOP language.
; It's a closure that keeps internal state and has named methods that mutate it.
(define-type DispatchObject (-> Symbol Any * Any))
(define-type Stack DispatchObject)
(define-type Machine DispatchObject)
(define-type Register DispatchObject)

(define-type ControllerText (Listof Instruction))
(define-type RegisterName Symbol)
(define-type MachineOpResult Any)
(define-type MachineOp (-> Any))
(define-type LabelName Symbol)
(define-type LabelVal InstructionOps)
(define-type Label (Pairof LabelName LabelVal))
(define-type Labels (Listof Label))
(define-type PrimopExprHead (Pairof Symbol Symbol))
(define-type PrimopExprTail (Listof MExpr))
(define-type PrimopExpr (Pairof PrimopExprHead PrimopExprTail))
(define-type PrimopImpl (-> MachineOpResult * MachineOpResult))
(define-type Primop (List Symbol PrimopImpl))
(define-type Primops (Listof Primop))

(define-type StackInst (Pairof Symbol (Pairof RegisterName Any)))

(define-type RegisterValue Any)
(struct reg ([name : RegisterName]) #:transparent)
(struct const ([ value : RegisterValue ]) #:transparent)
(struct boxed-const ([ value : RegisterValue ]) #:transparent)
(struct label ([ name : LabelName ]) #:transparent)
(struct op ([ name : Symbol] [ args : MExprs ]) #:transparent)
(struct eth-stack () #:transparent)
(define stack (eth-stack))
(define stack? eth-stack?)
(define-type MExpr (U reg
                      const
                      boxed-const
                      label
                      op))
(define-type MExprs (Listof MExpr))

(define-type InstLabel         Symbol)
(struct assign ([ reg-name : RegisterName ] [ value-exp : MExpr ]) #:transparent)
(struct test ([ condition : MExpr ]) #:transparent)
(struct branch ([ dest : MExpr ]) #:transparent)
(struct goto ([ dest : MExpr ]) #:transparent)
(struct save ([ reg-name : RegisterName ]) #:transparent)
(struct restore ([ reg-name : RegisterName ]) #:transparent)
(struct perform ([ action : op ]) #:transparent)
(struct am-asm ([ insts : EthInstructions ]))
(define-type Instruction (U InstLabel
                            assign
                            test
                            branch
                            goto
                            save
                            restore
                            perform
                            PyrAsm
                            ))
(define-type Instructions (Listof Instruction))
(define-type InstructionOp (MPairof Instruction MachineOp))
(define-type InstructionOps (Listof InstructionOp))

(define-type Frame (Pairof (MListof VariableName) (MListof Value)))
(define-type Environment (MListof Frame))

(define-type Values (Listof Value))

(struct primitive ([ implementation : Procedure ]))
(struct procedure ([ parameters : VariableNames ] [ body : Sequence ] [ environment : Environment ]))

; Code generator
(define-type Address Fixnum)
(struct eth-asm     ([ name : Symbol ]) #:transparent)
(struct eth-push    ([ size : (U 'shrink Fixnum) ] [ value : (U Integer Symbol) ]) #:transparent)
(struct eth-unknown ([ byte : Fixnum ]) #:transparent)
(define-type EthInstruction     (U eth-asm eth-push eth-unknown label))
(define-type EthInstructions    (Listof   EthInstruction))
(define-type (Generator  A)     (-> A     EthInstructions))
(define-type (Generator2 A B)   (-> A B   EthInstructions))
(define-type (Generator3 A B C) (-> A B C EthInstructions))

(struct opcode ([ byte : Fixnum ] [ name : Symbol ] [ num-reads : Fixnum ] [ num-writes : Fixnum ]) #:transparent)

(define-type EthWord Integer)

(struct evm ([ step : Fixnum ]
             [ bytecode : Bytes ]
             [ pc : Fixnum ]
             [ stack : (Listof EthWord) ]
             [ memory : Bytes ]
             [ gas : Fixnum ]
             [ halted? : Boolean ]
             [ largest-accessed-memory : Fixnum ]
             [ on-simulate : (-> evm opcode (Listof EthWord) Void) ]
             [ on-return : (-> evm Bytes Void) ]
             [ on-error : (-> evm Void) ]
             )
  #:mutable
  )

; Exception types
(struct exn:evm exn:fail ([ vm : evm ]))
(struct exn:evm:misaligned-addr exn:evm ([ addr : EthWord ]))
(struct exn:evm:throw exn:evm ([ value : Bytes ]))
(struct exn:evm:did-not-halt exn:evm ([ max-iterations : Fixnum ]))
(struct exn:evm:stack-underflow exn:evm ([ num-elements : Fixnum ] [ stack : (Listof EthWord )]))
(struct exn:evm:misaligned-jump exn:evm ([ addr : EthWord ]))

(struct simulation-result ([ vm : evm ] [ val : Bytes ]))
