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
(define-type PyrDefinition (List 'define (Pairof VariableName VariableNames) Pyramid))
(define-type PyrIf         (List 'if     Pyramid Pyramid (U Nothing Pyramid)))
(define-type PyrLambda     (List 'lambda VariableNames Sequence))
(define-type PyrBegin      (List 'begin  (Listof Pyramid)))
(define-type PyrCondClause (U PyrCondPred PyrCondElse))
(define-type PyrCondPred   (Pairof Pyramid Sequence))
(define-type PyrCondElse   (Pairof 'else Sequence))
(define-type PyrCond       (Pairof 'cond (Listof PyrCondClause)))
(define-type PyrApplication (Pairof Pyramid Sequence))
(define-type Pyramid (U PyrSelfEvaluating
                        PyrVariable
                        PyrQuote
                        PyrAssign
                        PyrDefinition
                        PyrIf
                        PyrLambda
                        PyrBegin
                        PyrCond
                        PyrApplication))

(define-type Value Any)
(define-type Sequence (Listof Pyramid))
(define-type Assertion Any)

(define-type Linkage (U 'next 'return LabelName))
(define-type Target RegisterName)
(define-type iseq-or-label (U LabelName inst-seq))
(struct inst-seq ([ needs : RegisterNames ] [ modifies : RegisterNames ] [ statements : Instructions ]))

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
(struct reg ([name : RegisterName]))
(struct const ([ value : RegisterValue ]))
(struct label ([ name : LabelName ]))
(struct op ([ name : Symbol] [ args : (Listof MExpr) ]))
(define-type MExpr (U reg
                      const
                      label
                      op))

(define-type InstLabel         Symbol)
(define-type InstAssign  (List 'assign  (Pairof RegisterName (Listof MExpr))))
(define-type InstTest    (List 'test    MExpr))
(define-type InstBranch  (List 'branch  label Any))
(define-type InstGoto    (List 'goto    MExpr Any))
(define-type InstSave    (List 'save    RegisterName))
(define-type InstRestore (List 'restore RegisterName))
(define-type InstPerform (List 'perform op))
(define-type Instruction (U InstLabel
                            InstAssign
                            InstTest
                            InstBranch
                            InstGoto
                            InstSave
                            InstRestore
                            InstPerform))
(define-type Instructions (Listof Instruction))
(define-type InstructionOp (MPairof Instruction MachineOp))
(define-type InstructionOps (Listof InstructionOp))

(define-type Frame (Pairof (MListof VariableName) (MListof Value)))
(define-type Environment (MListof Frame))

(define-type Values (Listof Value))

(struct primitive ([ implementation : Procedure ]))
(struct procedure ([ parameters : VariableNames ] [ body : Sequence ] [ environment : Environment ]))
