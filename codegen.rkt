#lang typed/racket/no-check

(require "evaluator.rkt")
(require "types.rkt")
(require "interpreter.rkt")

; WORD = 0x20
 
; Registers -> Stack
;; The abstract machine uses 5 256-bit virtual registers
;; * env:      &(0 * WORD)
;; * proc:     STACK
;; * val:      STACK
;; * argl:     STACK
;; * continue: STACK
;; Addresses have 8-bit granularity, so we multiply by WORD so they don't overlap.

(define-type Address Fixnum)
(struct eth-simple  ([ name : Symbol ]))
(struct eth-push    ([ size : Fixnum ]) ([ value : Integer ]))
(struct eth-unknown ([ opcode : Fixnum ]))
(define-type EthInstruction (U EthSimple EthPush EthUnknown))
(define-type EthInstructions (Listof EthInstruction))
(define-type (Generator A) (-> A EthInstructions))
(define-type (Generator2 A B) (-> A B EthInstructions))
(define-type (Generator3 A B C) (-> A B C EthInstructions))

; Used to turn labels into code addresses
(define byte-index 0)
(define label-map '())

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
        (target (assign-reg-name i)))
    (cg-write (reg target) value)))

(define (cg-test i)
  (cg-mexpr (test-condition i)))

(define (cg-branch i)
  (cg-mexpr (branch-dest i))
  (eth-simple 'JUMPI))

(define (cg-goto i)
  (cg-mexpr (goto-dest i))
  (eth-simple 'JUMP))

(define (cg-save i)
  (cg-write (reg 'val) (reg (stack-inst-reg-name i))))

(define (cg-restore i)
  (cg-write (reg (stack-inst-reg-name i)) (reg 'val)))

(define (cg-perform i)
  (cg-op (perform-action i)))
  

(: cg-mexpr (Generator MExpr))
(define (cg-mexpr exp)
  (cond ((reg?   exp) (cg-mexpr-reg   exp))
        ((const? exp) (cg-mexpr-const exp))
        ((op?    exp) (cg-mexpr-op    exp))
        (else
         (error "Unknown mexpr - cg-mexpr" exp))))

(: cg-mexpr-reg   (Generator reg))
(define (cg-mexpr-reg exp)
  (cond ((eq? (reg-name dest) 'env) (cg-mexpr-env))
        (else                       (cg-mexpr-stack))))

(: cg-mexpr-const (Generator const))
(define (cg-mexpr-const val)
  (eth-push 32 val)) ; TODO: Dynamically adjust size based on value

(: cg-mexpr-op    (Generator op))
(define (cg-mexpr-op exp)
  (let ((name (op-name exp))
        (args (op-args exp)))
    (cond ((eq? name 'compiled-procedure-entry)  (compiled-procedure-entry  (car args)))
          ((eq? name 'compiled-procedure-env)    (compiled-procedure-env    (car args)))
          ((eq? name 'primitive-procedure?)      (primitive-procedure?      (car args) (cadr args)))
          ((eq? name 'apply-primitive-procedure) (apply-primitive-procedure (car args) (cadr args)))
          ((eq? name 'lookup-variable-value)     (lookup-variable-value     (car args) (cadr args)))
          ((eq? name 'define-variable!)          (define-variable!          (car args) (cadr args) (caddr args)))
          ((eq? name 'false?)                    (false?                    (car args)))
          ((eq? name 'list)                      (list                      (car args)))
          (else
           (error "Unknown primitive op - cg-mexpr-op" exp)))))
          
(: cg-mexpr-stack (Generator Nothing))
(define cg-mexpr-stack (void))

(: cg-mexpr-env   (Generator Nothing))
(define cg-mexpr-env (cg-read-address (const #x0)))

(: cg-read-address (Generator MExpr))
(define (cg-read-address addr)
  (append (cg-mexpr addr)
          (eth-simple 'MLOAD)))

; Writes the top of the stack to the given destination
(: cg-write (Generator2 MExpr))
(define (cg-write dest)
  (cond ((reg? exp) (cg-write-reg dest))
        (else
         (error "Can only write to registers - cg-write" dest))))

(: cg-write-reg (Generator2 reg))
(define (cg-write-reg dest)
  (cond ((eq? (reg-name dest) 'env) (cg-write-env))
        (else                       (cg-write-stack))))

(: cg-write-env (Generator Nothing))
(define cg-write-env
  (cg-write-address (const #x0)))

(: cg-write-stack (Generator Nothing))
(define cg-write-stack (void))

(: cg-write-address (Generator MExpr))
(define (cg-write-address dest)
  (append (cg-mexpr dest)
          (eth-simple 'MSTORE)))
  

; Primitive operations on the EVM
(: cg-op-compiled-procedure-entry  (Generator  MExpr))
(: cg-op-compiled-procedure-env)   (Generator  MExpr))
(: cg-op-primitive-procedure?      (Generator2 MExpr MExpr))
(: cg-op-apply-primitive-procedure (Generator2 MExpr MExpr))
(: cg-op-lookup-variable-value     (Generator2 MExpr MExpr))
(: cg-op-define-variable!          (Generator3 MExpr MExpr MExpr))
(: cg-op-false?                    (Generator  MExpr))
(: cg-op-list                      (Generator  MExpr))

; Record that a label is located at a specific Ethereum bytecode offset.
(: mark-label (-> LabelName Address Void))
(define (mark-label name addr)
  (push-mlist! label-map (cons name addr)))
