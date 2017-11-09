#lang typed/racket

; Implements an abstract machine that can be interpreted.
; Other modules may translate the instructions for this machine
; to run compiled Scheme on a new architecture.

(require typed/racket/unsafe)
(require rnrs/mutable-pairs-6)
(require "ast.rkt")
(unsafe-require/typed "unsafe.rkt" [ unsafe-cast (All (A B) (-> A B)) ])
(provide (all-defined-out))

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
(struct label ([ label : LabelName ]))
(struct op ([ name : Symbol] [ args : (Listof MExpr) ]))
(define-type MExpr (U reg
                      const
                      label
                      op))

(define-type InstLabel Symbol)
(define-type InstAssign (Pairof Symbol (Pairof RegisterName MExpr)))
(define-type InstTest (Pairof Symbol MExpr))
(define-type InstBranch (Pairof Symbol (Pairof label Any)))
(define-type InstGoto (Pairof Symbol (Pairof MExpr Any)))
(define-type InstSave StackInst)
(define-type InstRestore StackInst)
(define-type InstPerform (Pairof Symbol PrimopExpr))
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

(: make-machine (-> (Listof RegisterName) Primops ControllerText Machine))
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                (machine 'allocate-register register-name))
              register-names)
    (machine 'install-operations ops)    
    (machine 'install-instruction-sequence (assemble controller-text machine))
    machine))

(: make-register (-> Symbol Register))
(define (make-register name)
  (let ([#{contents : Any} '*unassigned* ])
    (define (dispatch message . value)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) (set! contents value))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(: get-contents (-> Register RegisterValue))
(define (get-contents register)
  (register 'get null))

(: set-contents! (-> Register RegisterValue Void))
(define (set-contents! register value)
  (cast (register 'set value) Void))

(: pop (-> Stack Value))
(define (pop stack)
  (stack 'pop null))

(: push (-> Stack Value Void))
(define (push stack value)
  (cast (stack 'push value) Void))

(: make-stack (-> Stack))
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message . args)
      (cond ((eq? message 'push) push args)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(: make-new-machine (-> Machine))
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence (ann '() InstructionOps)))
    (let ((the-ops
            (list (list (ann 'initialize-stack Symbol)
                        (ann (lambda (args) (stack 'initialize null)) (-> (Listof Any) Any)))
                  ;;**next for monitored stack (as in section 5.2.4)
                  ;;  -- comment out if not wanted
                  (list 'print-stack-statistics
                        (lambda (args) (stack 'print-statistics null)))))
          (register-table
           (list (cons (ann 'pc Symbol) pc)
                 (cons 'flag flag))))
      (: allocate-register (-> Symbol Void))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (cons name (make-register name))
                        register-table))))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (car val)
              (error "Unknown register:" name))))
      (: execute (-> Void))
      (define (execute)
        (let ((insts (unsafe-cast (get-contents pc))))
          (if (null? insts)
              (void)
              (begin
                (instruction-execution-proc (car insts))
                (execute)))))
      (define (dispatch message . args)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence (unsafe-cast seq))))
              ((eq? message 'allocate-register) (allocate-register (cast args Symbol)))
              ((eq? message 'get-register) (lookup-register (cast args Symbol)))
              ((eq? message 'install-operations)
               (set! the-ops (append the-ops (cast args Primops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(: start (-> Machine Void))
(define (start machine)
  (void (machine 'start)))

(: get-register-contents (-> Machine Symbol Value))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(: set-register-contents! (-> Machine Symbol Value Void))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value))

(: get-register (-> Machine Symbol Register))
(define (get-register machine reg-name)
  (cast (machine 'get-register reg-name) Register))

(: assemble (-> ControllerText Machine InstructionOps))
(define (assemble controller-text machine)
  (let ((result (extract-labels controller-text)))
    (let ((insts (car result)) (labels (cdr result)))
      (update-insts! insts labels machine)
      insts)))

(: extract-labels (-> ControllerText (Pairof (Listof InstructionOp) (Listof Label))))
(define (extract-labels text)
  (if (null? text)
      (cons '() '())
      (let ((result (extract-labels (cdr text))))
        (let ((insts (car result)) (labels (cdr result)))
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (cons insts
                      (cons (make-label-entry next-inst insts) labels))
                (cons (cons (make-instruction next-inst) insts)
                      labels)))))))

(: get-stack (-> Machine Stack))
(define (get-stack machine)
  (cast (machine 'stack) Stack))

(: get-ops (-> Machine Primops))
(define (get-ops machine)
  (cast (machine 'operations null) Primops))

(: update-insts! (-> InstructionOps Labels Machine Void))
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (get-stack machine))
        (ops (get-ops machine)))
    (for-each
     (lambda ([ inst : InstructionOp ])
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(: make-instruction (-> Instruction InstructionOp))
(define (make-instruction text)
  (mcons text (lambda () (void))))

(: instruction-text (-> InstructionOp Instruction))
(define (instruction-text inst)
  (mcar inst))

(: instruction-execution-proc (-> InstructionOp MachineOp))
(define (instruction-execution-proc inst)
  (mcdr inst))

(: set-instruction-execution-proc! (-> InstructionOp MachineOp Void))
(define (set-instruction-execution-proc! inst proc)
  (set-mcdr! inst proc))

(: make-label-entry (-> LabelName InstructionOps Label))
(define (make-label-entry label-name insts)
  (cons label-name insts))

(: lookup-label (-> Labels LabelName LabelVal))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(: make-execution-procedure (-> Instruction Labels Machine Register Register Stack Primops MachineOp))
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (let ((i (car (cast inst (Pairof Symbol Any)))))
    (cond ((eq? i 'assign)
           (make-assign (cast inst InstAssign) machine labels ops pc))
          ((eq? i 'test)
           (make-test (cast inst InstTest) machine labels ops flag pc))
          ((eq? i 'branch)
           (make-branch (cast inst InstBranch) machine labels flag pc))
          ((eq? i 'goto)
           (make-goto (cast inst InstGoto) machine labels pc))
          ((eq? i 'save)
           (make-save (cast inst InstSave) machine stack pc))
          ((eq? i 'restore)
           (make-restore (cast inst InstRestore) machine stack pc))
          ((eq? i 'perform)
           (make-perform (cast inst InstPerform) machine labels ops pc))
          (else (error "Unknown instruction type -- ASSEMBLE"
                       inst)))))

(: make-assign (-> InstAssign Machine Labels Primops Register MachineOp))
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (op? value-exp)
               (make-operation-exp
                 value-exp machine labels operations)
               (make-primitive-exp
                 value-exp machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(: assign-reg-name (-> InstAssign RegisterName))
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(: assign-value-exp (-> InstAssign MExpr))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(: advance-pc (-> Register Void))
(define (advance-pc pc)
  (set-contents! pc (cdr (unsafe-cast (get-contents pc)))))

(: make-test (-> InstTest Machine Labels Primops Register Register MachineOp))
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (op? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(: test-condition (-> InstTest MExpr))
(define (test-condition test-instruction)
  (cdr test-instruction))

(: make-branch (-> InstBranch Machine Labels Register Register MachineOp))
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label? dest)
        (let ((insts
               (lookup-label labels (label-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(: branch-dest (-> InstBranch label))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(: make-goto (-> InstGoto Machine Labels Register MachineOp))
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label? dest)
           (let ((insts
                  (lookup-label labels
                                (label-label dest))))
             (lambda () (set-contents! pc insts))))
          ((reg? dest)
           (let ((reg
                  (get-register machine
                                (reg-name dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(: goto-dest (-> InstGoto MExpr))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(: make-save (-> InstSave Machine Stack Register MachineOp))
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(: make-restore (-> InstRestore Machine Stack Register MachineOp))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(: stack-inst-reg-name (-> StackInst RegisterName))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(: make-perform (-> InstPerform Machine Labels Primops Register MachineOp))
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (op? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(: perform-action (-> InstPerform PrimopExpr))
(define (perform-action inst) (cdr inst))

(: make-primitive-exp (-> MExpr Machine Labels MachineOp))
(define (make-primitive-exp exp machine labels)
  (cond ((const? exp)
         (let ((c (const-value exp)))
           (lambda () c)))
        ((label? exp)
         (let ((insts
                (lookup-label labels
                              (label-label exp))))
           (lambda () insts)))
        ((reg? exp)
         (let ((r (get-register machine
                                (reg-name exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(: make-operation-exp (-> op Machine Labels Primops MachineOp))
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (op-name exp) operations))
        (aprocs
         (map (lambda ([e : MExpr ])
                (make-primitive-exp e machine labels))
              (op-args exp))))
    (lambda ()
      (apply op (map (lambda ([p : MachineOp]) (p)) aprocs)))))

(: lookup-prim (-> Symbol Primops PrimopImpl))
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

'(REGISTER SIMULATOR LOADED)
