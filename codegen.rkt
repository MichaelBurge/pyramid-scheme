#lang typed/racket/no-check

(require "types.rkt")
(require "interpreter.rkt")

; WORD = 0x20
 
; Registers -> Stack
;; The abstract machine uses 5 256-bit virtual registers
;; * env:      &(0 * WORD)
;; * proc:     &(1 * WORD)
;; * val:      &(2 * WORD)
;; * argl:     &(3 * WORD)
;; * continue: &(4 * WORD)
;; Addresses have 8-bit granularity, so we multiply by WORD so they don't overlap.

(define-type Address Fixnum)
(struct eth-simple  ([ name : Symbol ]))
(struct eth-push    ([ size : Fixnum ]) ([ value : Integer ]))
(struct eth-unknown ([ opcode : Fixnum ]))
(define-type EthInstruction (U EthSimple EthPush EthUnknown))
(define-type EthInstructions (Listof EthInstruction))
(define-type (Generator A) (-> A EthInstructions))
(define-type (Generator2 A B) (-> A B EthInstructions))

; Used to turn labels into code addresses
(define byte-index 0)

(: codegen (Generator Instructions))
(define (codegen is)
  (if (null? is)
      '()
      (append (codegen-one (car is))
              (codegen     (cdr is)))))
       

(: codegen-one (Generator Instruction))
(define (codegen-one i)
  (cond ((label?   i) (codegen-label   i))
        ((assign?  i) (codegen-assign  i))
        ((test?    i) (codegen-test    i))
        ((branch?  i) (codegen-branch  i))
        ((goto?    i) (codegen-goto    i))
        ((save?    i) (codegen-save    i))
        ((restore? i) (codegen-restore i))
        ((perform? i) (codegen-perform i))
        (else
         (error "Unknown instruction type -- CODEGEN" i))))

(: cg-label   (Generator InstLabel)
(: cg-assign  (Generator InstAssign))
(: cg-test    (Generator InstTest))
(: cg-branch  (Generator InstBranch))
(: cg-goto    (Generator InstGoto))
(: cg-save    (Generator InstSave))
(: cg-restore (Generator InstRestore))
(: cg-perform (Generator InstPerform))

(define (cg-label i)
  (mark-label (label-name i))
  (eth-simple 'JUMPDEST)))
  
(define (cg-assign i)
  (let ((value   (assign-value-exp i))
        (address (assign-reg-name i)))
    (cg-write-address address value)))

(define (cg-test i)
  

(: cg-mexpr (Generator MExpr))
(: cg-read-address (Generator MExpr))
(: cg-write-address (Generator2 MExpr MExpr))

; Emit a single Ethereum opcode
   (: emit (-> Symbol Void))
(: resolve-value-exp (-> MExprs MExpr))
; Remember that a labelis located at a specific Ethereum bytecode offset.
(: mark-label (-> Symbol Address Void))
(: target-address (-> Target Address))
