#lang typed/racket

(require (submod "types.rkt" abstract-machine))
(require (submod "types.rkt" evm-assembly))
(require "types.rkt")
(require "utils.rkt")
(require "ast.rkt")
(require "globals.rkt")
(require racket/list)

(require "typed/binaryio.rkt")

(provide (all-defined-out)
         (all-from-out (submod "types.rkt" evm-assembly)))

#|
-- Registers -> Stack --
The abstract machine uses 5 256-bit virtual registers
* env:      &(1 * WORD)
* proc:     &(2 * WORD)
* continue: &(3 * WORD)
* argl:     &(4 * WORD)
* val:      &(5 * WORD)

Addresses have 8-bit granularity, so we multiply by WORD so they don't overlap.
Arguments to procedures are passed on the stack. The first argument is the first stack entry.

-- Runtime Representation --
General Scheme objects must be "boxed" into a pointer.
Unboxed numbers could also be manipulated, but cannot be queried for their type and have no memory address.
In particular, unboxed numbers cannot be returned as the final program result, because RETURN acts on memory.

Boolean values are unboxed, since they are usually immediately consumed. They can be explicitly boxed if necessary.

Boxed values are pointers to a tag. Depending on the tag, additional data follows:
 * 0: Fixnum:              1 word      - The number's value
 * 1: Symbol:              1 word      - 32 8-bit ASCII characters
 * 2: Compiled Procedure:  2 words     - A code pointer, and closure environment pointer
 * 3: Primitive Procedure: 1 word      - A code pointer
 * 4: Pair:                2 words     - Pointer to first element, pointer to second element
 * 5: Vector:              1 + n words - A size n, followed by n pointers to individual elements
 * 6: Nil:                 0 words     - Only the tag is used.
 * 7: Continuation:        2 words     - continue register, env register
Additionally, there are derived objects used in the standard library:
 * Environment: (pair frame (Nil | enclosing-environment))
 * Frame:       (pair vars  vals)
 * List:        Nil | (pair X List)

-- Assembly --
Some procedures are written in EVM assembly. To help validate them, they are annotated as follows:
* Arguments are first-to-last on the stack.
* Each line is annotated with the expected stack difference from the method beginning.
* A goto must arrive on a line with the same stack size.
* A branch pops one element, and then arrives on a line with the same stack size.
* The procedure must terminate with a stack size of 0

-- Primitive operations --
There is a standard library of primitive operations.
* Primitive operations are used by the abstract machine code.
* Some primitive operations are inlined. Their linkage is implicitly 'next.
* Others have a single copy placed near the start of the program. It costs 2 additional JUMP instructions.

-- Optimizations --
These optimizations are currently unimplemented:
* Values that are allocated and used within a lexical scope should not have infinite extent. Roll back the allocator pointer.

-- Constants --
* Integers are converted into push instructions.
* Quotes are always variable names. Labels have their own classification in MExpr.
* Quotes are converted into 256-bit integers by treating up to 32 characters as 8-bit ASCII.
* Lists emits a series of (cons) calls corresponding to the elements.
|#

(define-namespace-anchor *asm-anchor*)

; To avoid excessive inlining, a single copy of these is emitted.
(define *label-op-lookup-variable-value* (make-label 'op-lookup-variable-value))
(define *label-op-define-variable!*      (make-label 'op-define-variable!))
(define *label-op-set-variable-value!*   (make-label 'op-set-variable!))
(define *label-op-return*                (make-label 'op-return))
(define *label-op-restore-continuation*  (make-label 'op-restore-continuation))
(define *label-op-primitive-procedure?*  (make-label 'op-primitive-procedure?))
(define *label-op-compiled-procedure?*   (make-label 'op-compiled-procedure?))
(define *label-op-continuation?*         (make-label 'op-primitive-procedure?))


; Constants
(define TAG-FIXNUM              0)
(define TAG-SYMBOL              1)
(define TAG-COMPILED-PROCEDURE  2)
(define TAG-PRIMITIVE-PROCEDURE 3)
(define TAG-PAIR                4)
(define TAG-VECTOR              5)
(define TAG-NIL                 6)
(define TAG-CONTINUATION        7)

(define MEM-ENV           #x20)
(define MEM-PROC          #x40)
(define MEM-CONTINUE      #x60)
(define MEM-ARGL          #x80)
(define MEM-VAL           #xa0)
(define MEM-NIL           #xc0)
(define MEM-ALLOCATOR     #xe0)
(define MEM-DYNAMIC-START #x100) ; This should be the highest hardcoded memory address.

(define WORD       #x20) ; 256-bit words / 8 bit granularity addresses = 32 8-bit words, or 0x20.

; Top-level code generator
(: codegen (Generator Instructions))
(define (codegen is)
  (*symbol-table* (make-symbol-table))
  (*relocation-table* (make-relocation-table))
  (*reverse-symbol-table* (make-reverse-symbol-table))
  (*asm-namespace*) (namespace-anchor->namespace *asm-anchor*)
  
  (append
   (cg-initialize-program)
   (cg-define-primops)
   (codegen-list is)
   (op-return (reg 'val))))

(: codegen-list (Generator Instructions))
(define (codegen-list is)
  (apply append (map codegen-one is)))

(: codegen-one (Generator Instruction))
(define (codegen-one i)
  (*abstract-offset* (+ 1 (*abstract-offset*)))
  (cond ((label?      i) (cg-label   i))
        ((symbol?  i) (error "Unexpected symbol - codegen-one" i))
        ((assign?  i) (cg-assign  i))
        ((test?    i) (cg-test    i))
        ((branch?  i) (cg-branch (branch-dest i) stack))
        ((goto?    i) (cg-goto (goto-dest i)))
        ((save?    i) (cg-save    i))
        ((restore? i) (cg-restore i))
        ((perform? i) (cg-perform i))
        ((pyr-asm? i) (cg-asm i))
        (else
         (error "Unknown instruction type -- codegen-one:" i))))

(: cg-label (Generator label-definition))
(define (cg-label i) (list i))

(: cg-assign  (Generator assign))
(define (cg-assign i)
  (let ((value : MExpr (assign-value i))
        (target : RegisterName (assign-reg-name i)))
    (append
     (debug-label 'cg-assign)
     (cg-write-reg (reg target) value)
     (debug-label 'cg-assign-end))))

(: cg-test    (Generator test))
(define (cg-test i)
  (append
   (debug-label 'cg-test)
   (cg-mexpr (test-condition i))
   (debug-label 'cg-test-end)))

(: cg-save    (Generator save))
(define (cg-save i)
  (append
   (debug-label 'cg-save)
   (cg-write-stack (reg (save-reg-name i)))
   (debug-label 'cg-save-end)))

(: cg-restore (Generator restore))
(define (cg-restore i)
  (append
   (debug-label 'cg-restore)
   (cg-write-reg (reg (restore-reg-name i)) stack)
   (debug-label 'cg-restore-end)))

(: cg-perform (Generator perform))
(define (cg-perform i)
  (append
   (debug-label 'cg-perform)
   (cg-mexpr-op (perform-action i))
   (debug-label 'cg-perform-end)))

(module asm-unsafe typed/racket/no-check
  (require "types.rkt")
  (provide eval-asm-cg)
  (define (eval-asm-cg anchor i) (eval i (namespace-anchor->namespace anchor)))
  )
  

(require/typed 'asm-unsafe
  [ eval-asm-cg (Generator2 Namespace-Anchor Any)])

(: eval-asm (Generator pyr-asm-base))
(define (eval-asm i)
  (match i
    [(struct pyr-asm-push  (size n)) (listof (eth-push size n))]
    [(struct pyr-asm-op    (op))     (listof (eth-asm op))]
    [(struct pyr-asm-bytes (value))  (map eth-unknown (bytes->list value))]
    [(struct label-definition _)     (listof i)]
    [(struct pyr-asm-cg (exp))       (eval-asm-cg *asm-anchor* exp)]))

(: cg-asm (Generator pyr-asm))
(define (cg-asm is)
  (apply append (map eval-asm (pyr-asm-insts is))))

;;; Primitive operations emitted by the abstract compiler

(: op-lookup-variable-value (Generator2 MExpr MExpr))
(define (op-lookup-variable-value name env)     (cg-invoke-primop *label-op-lookup-variable-value* name env))

(: op-define-variable! (Generator3 MExpr MExpr MExpr))
(define (op-define-variable! name value env)    (cg-invoke-primop *label-op-define-variable!* name value env))

(: op-set-variable-value! (Generator3 MExpr MExpr MExpr))
(define (op-set-variable-value! name value env) (cg-invoke-primop *label-op-set-variable-value!* name value env))

(: op-return (Generator MExpr))
(define (op-return value)                       (cg-invoke-primop *label-op-return* value))

(: op-restore-continuation (Generator MExpr))
(define (op-restore-continuation cont)          (cg-invoke-primop *label-op-restore-continuation* cont))

(: op-compiled-procedure? (Generator MExpr))
(define (op-compiled-procedure? obj)            (cg-invoke-primop *label-op-compiled-procedure?* obj))

(: op-continuation? (Generator MExpr))
(define (op-continuation? obj)                  (cg-invoke-primop *label-op-continuation?* obj))

(: op-primitive-procedure? (Generator MExpr))
(define (op-primitive-procedure? obj)           (cg-invoke-primop *label-op-primitive-procedure?* obj))

(: cg-op-box                       (Generator  MExpr))       ; Creates a boxed integer or symbol.
(define (cg-op-box exp)
  (append
   (debug-label 'cg-op-box)
   (if (const? exp)
       (cond ((integer? (const-value exp)) (cg-make-fixnum exp))
             ((symbol?  (const-value exp)) (cg-make-symbol exp))
             (else
              (error "cg-op-box: Unsupported boxed constant" exp)))
       (error "cg-op-box: Can only box constants" exp))))

(: cg-op-extend-environment        (Generator3 MExpr MExpr MExpr)) ; Adds a frame to the environment.
(define (cg-op-extend-environment vars vals env)
  (append
   (debug-label 'cg-op-extend-environment)
   (cg-intros (list vars vals env)) ; [ vars; vals; env ]
   (cg-make-pair stack stack)       ; [ frame; env ]
   (cg-make-pair stack stack)))     ; [ env' ]

(: cg-op-make-compiled-procedure   (Generator2 MExpr MExpr)) ; A lambda creates a runtime value from a code pointer & closure.
(define (cg-op-make-compiled-procedure code env)
  (append
   (debug-label 'cg-op-make-compiled-procedure)
   (cg-make-compiled-procedure code env)))

(: cg-op-compiled-procedure-entry  (Generator  MExpr))       ; The code pointer from a make-compiled-procedure object.
(define (cg-op-compiled-procedure-entry obj)
  (append
   (debug-label 'cg-op-compiled-procedure-entry)
   (cg-read-address-offset obj (const 1))))

(: cg-op-compiled-procedure-env    (Generator  MExpr))       ; The environment address from a make-compiled-procedure object.
(define (cg-op-compiled-procedure-env obj)
  (append
   (debug-label 'cg-op-compiled-procedure-env)
   (cg-read-address-offset obj (const 2))))

(: cg-op-compiled-procedure?       (Generator MExpr))
(define (cg-op-compiled-procedure? obj)
  (append
   (debug-label 'cg-op-compiled-procedure?)        ; [ ]
   (cg-tag obj)                                    ; [ tag ]
   (cg-eq? (const TAG-COMPILED-PROCEDURE) stack))) ; [ eq? ]

(: cg-op-continuation?             (Generator  MExpr))
(define (cg-op-continuation? obj)
  (append
   (debug-label 'cg-op-continuation?)
   (cg-tag obj)
   (cg-eq? (const TAG-CONTINUATION) stack)))

(: cg-op-primitive-procedure?      (Generator  MExpr))       ; Is the object at an address a primitive procedure?
(define (cg-op-primitive-procedure? obj)
  (append
   (debug-label 'cg-op-primitive-procedure?)
   (cg-tag obj)
   (cg-eq? (const TAG-PRIMITIVE-PROCEDURE) stack)))

(: cg-op-primitive-procedure-entry (Generator MExpr))
(define (cg-op-primitive-procedure-entry obj)
  (append
   (debug-label 'cg-op-primitive-procedure-entry)
   (cg-read-address-offset obj (const 1))))

(: cg-op-apply-primitive-procedure (Generator2 MExpr MExpr)) ; Calls the primitive procedure with a code pointer and argument list.
(define (cg-op-apply-primitive-procedure proc argl)
  (let ((after-op (make-label 'after-op)))
    (append
     (debug-label 'cg-op-apply-primitive-procedure)
     (cg-intros (list proc argl after-op))
     ; Arguments passed as a dynamic list.
     (cg-op-primitive-procedure-entry stack) ; [ primop-entry; args; return ]
     (asm 'JUMP)
     `(,after-op))))

(: cg-op-cons                      (Generator2 MExpr MExpr))
(define (cg-op-cons a b)
  (append (debug-label 'cg-op-cons)
          (cg-cons a b)))

; Could be implemented in Pyramid, but lookup-variable-value would be needed to lookup that definition.
; PSEUDOCODE:

#|
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (if (null? vals)
          (env-loop (cdr env))
          (if (eq? var (car vars))
              (car vals)
              (scan (cdr vars) (cdr vals)))))
    (if (null? env)
        (error "Nonexistent variable:" var)
        (let ((frame (car env)))
          (scan (car frame)
                (cdr frame)))))
  (env-loop env))
|#
(: cg-op-lookup-variable-value (Generator2 MExpr MExpr))
(define (cg-op-lookup-variable-value name env)
  (let ((env-loop    (make-label 'lookup-variable-value-env-loop))
        (scan        (make-label 'lookup-variable-value-scan))
        (scan-else-1 (make-label 'lookup-variable-value-scan-else-1))
        (scan-else-2 (make-label 'lookup-variable-value-scan-else-2))
        (term        (make-label 'lookup-variable-value-term))
        (not-found   (make-label 'lookup-variable-value-not-found))
        )
    (append
     (debug-label 'cg-op-lookup-variable-value)
     (cg-intros (list name env))
     ; Stack: [ var, env ]                          ; len = 2
     `(,env-loop)
     (asm 'DUP2)    ; [ env; name; env ]
     (cg-null? stack) ; [ null?; name; env ]
     (cg-branch not-found stack) ; [ name; env ]
     (asm 'DUP2)    ; [ env; name; env ]
     (cg-car stack) ; [ frame; name; env ]
     (asm 'DUP1)    ; [ frame; frame; name; env ]
     (cg-car stack) ; [ fvars; frame; name; env ]
     (asm 'SWAP1)   ; [ frame; fvars; name; env ]
     (cg-cdr stack) ; [ fvals; fvars; name; env ]
     ; Stack: [ fvals, fvars, var, env ]
     `(,scan)
     (asm 'DUP1)      ; [ fvals; fvals; fvars; name; env ]
     (cg-null? stack) ; [ null?; fvals; fvars; name; env ]
     (asm 'ISZERO)    ; [ !null?; fvals; fvars; name; env ]
     (cg-branch scan-else-1 stack) ; [ fvals; fvars; name; env ]
     (cg-pop 2)       ; [ name; env ]
     (asm 'SWAP1)     ; [ env; name ]
     (cg-cdr stack)   ; [ env'; name ]
     (asm 'SWAP1)     ; [ name; env' ]
     (cg-goto env-loop) ; [ name; env' ]
     ; Stack: [ fvals, fvars, var, env ]
     `(,scan-else-1)
     (asm 'DUP2)       ; [ fvars; fvals; fvars; var; env ]
     (cg-car stack)    ; [ var'; fvals; fvars; var; env ]
     (asm 'DUP4)       ; [ var; var'; fvals; fvars; var; env ]
     (cg-eq? stack stack) ; [ eq? ; fvals; fvars; var; env ]
     (asm 'ISZERO)     ; [ !eq?; fvals; fvars; var; env ]
     (cg-branch scan-else-2 stack) ; [fvals; fvars; var; env ]
     (cg-car stack)    ; [ val; fvars; var; env ]
     (asm 'SWAP3)      ; [ fvars; var; env; val ]
     (cg-pop 3)        ; [ val ]
     (cg-goto term)    ; [ val ]
     ; Stack: [ fvals, fvars, var, env ]            ; len = 4
     `(,scan-else-2)
     (cg-cdr stack)    ; [ fvals; fvars; var; env ]
     (asm 'SWAP1)      ; [ fvars; fvals'; var; env ]
     (cg-cdr stack)    ; [ fvars'; fvals'; var; env ]
     (asm 'SWAP1)      ; [ fvals'; fvars'; var; env ]
     (cg-goto scan)    ; [ fvals'; fvars'; var; env ]
     `(,not-found)
     (asm 'REVERT)
     `(,term))))

#|
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (if (null? vals)
          (env-loop (cdr env))
          (if (eq? var (car vars))
              (set-car! vals val)
              (scan (cdr vars) (cdr vals)))))
    (if (null? env)
        (error "Nonexistent variable:" var)
        (let ((frame (car env)))
          (scan (car frame)
                (cdr frame)))))
  (env-loop env))
|#
(: cg-op-set-variable-value! (Generator3 MExpr MExpr MExpr))
(define (cg-op-set-variable-value! name value env)
  (let ([env-loop    (make-label 'set-variable-value-env-loop)]
        [scan        (make-label 'set-variable-value-scan)]
        [scan-else-1 (make-label 'set-variable-value-scan-else-1)]
        [scan-else-2 (make-label 'set-variable-value-scan-else-2)]
        [term        (make-label 'set-variable-value-term)]
        [not-found   (make-label 'set-variable-value-not-found)]
        )
    (append
     (debug-label 'cg-op-set-variable-value)
     (cg-intros (list name value env))
     ; Stack: [ var, value, env ]
     `(,env-loop)
     (asm 'DUP3)      ; [ env; name; value; env ]
     (cg-null? stack) ; [ null?; name; value; env ]
     (cg-branch not-found stack) ; [ name; value; env ]
     (asm 'DUP3)      ; [ env; name; value; env ]
     (cg-car stack)   ; [ frame; name; value; env ]
     (asm 'DUP1)      ; [ frame; frame; name; value; env ]
     (cg-car stack)   ; [ fvars; frame; name; value; env ]
     (asm 'SWAP1)     ; [ frame; fvars; name; value; env ]
     (cg-cdr stack)   ; [ fvals; fvars; name; value; env ]
     ; Stack: [ fvals, fvars, name, value, env ]
     `(,scan)
     (asm 'DUP1)      ; [ fvals; fvals; fvars; name; value; env ]
     (cg-null? stack) ; [ null?; fvals; fvars; name; value; env ]
     (asm 'ISZERO)    ; [ !null?; fvals; fvars; name; value; env ]
     (cg-branch scan-else-1 stack) ; [ fvals; fvars; name; value; env ]
     (cg-pop 2)       ; [ name; value; env ]
     (asm 'SWAP2)     ; [ env; value; name ]
     (cg-cdr stack)   ; [ env'; value; name ]
     (asm 'SWAP2)     ; [ name; value; env' ]
     (cg-goto env-loop) ; [ name; value; env' ]
     ; Stack: [ fvals, fvars, var, value, env ]
     `(,scan-else-1)
     (asm 'DUP2)      ; [ fvars; fvals; fvars; name; value; env ]
     (cg-car stack)   ; [ name'; fvals; fvars; name; value; env ]
     (asm 'DUP4)      ; [ name; name'; fvals; fvars; name; value; env ]
     (cg-eq? stack stack) ; [ eq?; fvals; fvars; name; value; env ]
     (asm 'ISZERO)    ; [ neq?; fvals; fvars; name; value; env ]
     (cg-branch scan-else-2 stack) ; [ fvals; fvars; name; value; env ]
     (asm 'DUP4)      ; [ value; fvals; fvars; name; value; env ]
     (cg-swap 1)      ; [ fvals; value; fvars; name; fvars; env ]
     (cg-set-car! stack stack) ; [ fvars; name; fvars; env ]
     (cg-pop 4)       ; [ ]
     (cg-goto term)   ; [ ]
     ; Stack: [ fvals, fvars, name, value, env ]
     `(,scan-else-2)
     (cg-cdr stack)    ; [ fvals; fvars; name; value; env ]
     (asm 'SWAP1)      ; [ fvars; fvals'; name; value; env ]
     (cg-cdr stack)    ; [ fvars'; fvals'; name; value; env ]
     (asm 'SWAP1)      ; [ fvals'; fvars'; name; value; env ]
     (cg-goto scan)    ; [ fvals'; fvars'; name; value; env ]
     `(,not-found)
     (asm 'REVERT)
     `(,term))))      ; []

; PSEUDOCODE:
;; (define (cg-op-define-variable! var val env)
;;   (let ((frame (car env)))
;;     (define (scan vars vals)
;;       (if (null? vars)
;;           (add-binding-to-frame! var val frame)
;;           (if (eq? var (car vars))
;;               (set-car! vals val)
;;               (scan (cdr vars) (cdr vals)))))))
;;     (scan (car frame)
;;           (cdr frame))))
(: cg-op-define-variable! (Generator3 MExpr MExpr MExpr))
(define (cg-op-define-variable! name value env)
  (let ((scan        (make-label 'define-variable!-scan))
        (scan-else-1 (make-label 'define-variable!-scan-else-1))
        (scan-else-2 (make-label 'define-variable!-scan-else-2))
        (term        (make-label 'define-variable!-term))
        )
    (append
     (debug-label 'cg-op-define-variable!)
     (cg-intros (list name value env))
     ; Stack:            [ name; value; env ]
     (asm 'DUP3)       ; [ env; name; value; env ]
     (cg-car stack)    ; [ frame; name; value; env ]
     ; Stack:            [ frame; name; value; env ]
     (asm 'DUP1)       ; [ frame; frame; name; value; env ]
     (cg-cdr stack)    ; [ fvals; frame; name; value; env ]
     (asm 'SWAP1)      ; [ frame; fvals; name; value; env ]
     (cg-car stack)    ; [ fvars; fvals; name; value; env ]
     ; Stack:            [ fvars; fvals; name; value; env ]
     `(,scan)
     (asm 'DUP1)       ; [ fvars; fvars; fvals; name; value; env ]
     (cg-null? stack)  ; [ null?; fvars; fvals; name; value; env ]
     (asm 'ISZERO)     ; [ !null?; fvars; fvals; name; value; env ]
     (cg-branch scan-else-1 stack) ; [ fvars; fvals; name; value; env ]
     (cg-pop 2)        ; [ name; value; env ]
     (asm 'SWAP2)      ; [ env; value; name ]
     (cg-car stack)    ; [ frame; value; name ]
     (asm 'SWAP2)      ; [ name; value; frame ]
     (cg-add-binding-to-frame stack stack stack) ; [ ]
     (cg-goto term) ; []
     ; Stack:               [ fvars; fvals; name; value; env ]
     `(,scan-else-1)
     (asm 'DUP3)          ; [ name; fvars; fvals; name; value; env ]
     (asm 'DUP2)          ; [ fvars; name; fvars; fvals; name; value; env ]
     (cg-car stack)       ; [ name'; name; fvars; fvals; name; value; env ]
     (cg-eq? stack stack) ; [ eq?; fvars; fvals; name; value; env ]
     (asm 'ISZERO)        ; [ neq?; fvars; fvals; name; value; env ]
     (cg-branch scan-else-2 stack) ; [ fvars; fvals; name; value; env ]
     ; Stack:               [ fvars; fvals; name; value; env ]
     (asm 'SWAP1)         ; [ fvals; fvars; name; value; env ]
     (asm 'DUP4)          ; [ value; fvals; fvars; name; value; env ]
     (cg-reverse 2)       ; [ fvals; value; fvars; name; value; env ]
     (cg-set-car! stack stack) ; [ fvars; name; value; env ]
     (cg-pop 4)           ; [ ]
     (cg-goto term)       ; [ ]
     ; Stack                [ fvars; fvals; name; value; env ]
     `(,scan-else-2)
     (cg-cdr stack)       ; [ fvars'; fvals; name; value; env ]
     (cg-reverse 2)       ; [ fvals; fvars'; name; value; env ]
     (cg-cdr stack)       ; [ fvals'; fvars'; name; value; env ]
     (cg-reverse 2)       ; [ fvars'; fvals'; name; value; env ]
     (cg-goto scan)       ; [ fvars'; fvals'; name; value; env ]
     `(,term))))

(: cg-op-false?                    (Generator  MExpr))       ; Evaluates to 1 if the expression is 0
(define (cg-op-false? exp)
  (append
   (debug-label 'cg-op-false?)
   (cg-mexpr exp)
   (asm 'ISZERO)))

(: cg-op-list                      (Generator  MExpr))       ; Creates a 1-element vector with the given object.
(define (cg-op-list exp)
  (append
   (debug-label 'cg-op-list)
   (cg-mexpr exp)          ; [ x ]
   (cg-make-nil)           ; [ nil; x ]
   (cg-reverse 2)          ; [ x; nil ] 
   (cg-cons stack stack))) ; [ x:nil]
  

(: cg-mexpr (Generator MExpr))
(define (cg-mexpr exp)
  (cond ((reg?   exp)  (cg-mexpr-reg   exp))
        ((const? exp)  (cg-mexpr-const exp))
        ((boxed-const? exp) (cg-mexpr-boxed-const exp))
        ((op?    exp)  (cg-mexpr-op    exp))
        ((symbol? exp) (cg-mexpr-label (label exp)))
        ((label? exp)  (cg-mexpr-label exp))
        ((stack? exp)  '())
        (else
         (error "cg-mexpr: Unknown mexpr" exp (list? exp)))))

(: cg-mexpr-reg   (Generator reg))
(define (cg-mexpr-reg dest)
  (let ((reg (reg-name dest)))
    (cond ((eq? reg 'env)      (cg-read-address (const MEM-ENV)))
          ((eq? reg 'proc)     (cg-read-address (const MEM-PROC)))
          ((eq? reg 'continue) (cg-read-address (const MEM-CONTINUE)))
          ((eq? reg 'argl)     (cg-read-address (const MEM-ARGL)))
          ((eq? reg 'val)      (cg-read-address (const MEM-VAL)))
          (else
           (error "cg-mexpr-reg: Unknown register" dest)))))

(: cg-write-reg (Generator2 reg MExpr))
(define (cg-write-reg dest exp)
  (let ((reg (reg-name dest)))
    (cond ((eq? reg 'env)      (cg-write-address (const MEM-ENV)  exp))
          ((eq? reg 'proc)     (cg-write-address (const MEM-PROC)  exp))
          ((eq? reg 'continue) (cg-write-address (const MEM-CONTINUE) exp))
          ((eq? reg 'argl)     (cg-write-address (const MEM-ARGL) exp))
          ((eq? reg 'val)      (cg-write-address (const MEM-VAL) exp))
          (else
           (error "cg-write-reg: Unknown register" dest exp)))))

(: cg-mexpr-const (Generator const))
(define (cg-mexpr-const exp)
  (let ((val (const-value exp)))
    (begin
      (cond ((symbol? val)
             (let ((int (symbol->integer val)))
               (list (eth-push (integer-bytes int) int))))
            ((integer? val) (list (eth-push (integer-bytes val) val)))
            ((list? val)    (cg-make-list (map const val) #f))
            (else
             (error "cg-mexpr-const: Unsupported constant" exp))))))
          

(: cg-mexpr-boxed-const (Generator boxed-const))
(define (cg-mexpr-boxed-const exp)
  (let ([ val (boxed-const-value exp) ])
    (cond [(symbol? val)
           (let ((int (symbol->integer (cast val Symbol))))
             (cg-make-symbol (const (integer-bytes int))))]
          [(integer? val) (cg-make-fixnum (const val))]
          [(list? val)    (cg-make-list (map const val) #t)]
          [else           (error "cg-mexpr-const: Unsupported constant" exp)])))
      

(: cg-mexpr-op    (Generator op))
(define (cg-mexpr-op exp)
  (let ((name (op-name exp))
        (args (op-args exp)))
    (match name
      ; Procedures
      ('make-compiled-procedure   (cg-op-make-compiled-procedure   (car args) (cadr args)))
      ('define-variable!          (op-define-variable!             (car args) (cadr args) (caddr args)))
      ('set-variable-value!       (op-set-variable-value!          (car args) (cadr args) (caddr args)))
      ('restore-continuation      (op-restore-continuation         (car args)))
      ; Expressions
      ('box                       (cg-op-box                       (car args)))
      ('extend-environment        (cg-op-extend-environment        (car args) (cadr args) (caddr args)))
      ('compiled-procedure-entry  (cg-op-compiled-procedure-entry  (car args)))
      ('compiled-procedure-env    (cg-op-compiled-procedure-env    (car args)))
      ('compiled-procedure?       (op-compiled-procedure?          (car args)))
      ('primitive-procedure?      (cg-op-primitive-procedure?      (car args)))
      ('continuation?             (op-continuation?                (car args)))
      ('apply-primitive-procedure (cg-op-apply-primitive-procedure (car args) (cadr args)))
      ('lookup-variable-value     (op-lookup-variable-value        (car args) (cadr args)))
      ('false?                    (cg-op-false?                    (car args)))
      ('list                      (cg-op-list                      (car args)))
      ('cons                      (cg-op-cons                      (car args) (cadr args)))
      (else
       (error "cg-mexpr-op: Unknown primitive op" name args)))))

(: cg-mexpr-label (Generator (U label-definition label)))
(define (cg-mexpr-label exp)
  (list (eth-push 'shrink exp)))

(: cg-mexpr-stack Generator0)
(define (cg-mexpr-stack) '())

(: cg-write-stack (Generator MExpr))
(define (cg-write-stack exp) (cg-mexpr exp))

(: cg-goto (Generator  MExpr))
(define (cg-goto dest)
  (append (cg-mexpr dest)
          (asm 'JUMP)))

(: cg-branch (Generator2 MExpr MExpr))
(define (cg-branch dest pred)
  (append (cg-intros (list dest pred))
          (asm 'JUMPI)))

(: cg-reverse (Generator Integer))
(define (cg-reverse size)
  (cond ((eq? size 2) (asm 'SWAP1))
        ((eq? size 3)  ; [ x1; x2; x3 ]
         (asm 'SWAP2)) ; [ x3; x2; x1 ]
        ((eq? size 4) ; [ x1; x2; x3; x4 ]
         (append
          (asm 'SWAP3) ; [ x4; x2; x3; x1 ]
          (asm 'SWAP1) ; [ x2; x4; x3; x1 ]
          (asm 'SWAP2) ; [ x3; x4; x2; x1 ]
          (asm 'SWAP1) ; [ x4; x3; x2; x1 ]
          ))
        (else
         (error "Unsupported size -- cg-reverse" size))))

(: cg-pop                  (Generator  Integer))
(define (cg-pop size)
  (if (eq? size 0)
      '()
      (cons (eth-asm 'POP)
            (cg-pop (- size 1)))))

(: cg-swap                 (Generator  Integer))
(define (cg-swap size)
  (cond ((eq? size  1) (asm 'SWAP1))
        ((eq? size  2) (asm 'SWAP2))
        ((eq? size  3) (asm 'SWAP3))
        ((eq? size  4) (asm 'SWAP4))
        ((eq? size  5) (asm 'SWAP5))
        ((eq? size  6) (asm 'SWAP6))
        ((eq? size  7) (asm 'SWAP7))
        ((eq? size  8) (asm 'SWAP8))
        ((eq? size  9) (asm 'SWAP9))
        ((eq? size 10) (asm 'SWAP10))
        ((eq? size 11) (asm 'SWAP11))
        ((eq? size 12) (asm 'SWAP12))
        ((eq? size 13) (asm 'SWAP13))
        ((eq? size 14) (asm 'SWAP14))
        ((eq? size 15) (asm 'SWAP15))
        ((eq? size 0)  '())
        (else (error "Unknown swap size -- cg-swap" size))))

(: cg-read-address         (Generator  MExpr))
(define (cg-read-address addr)
  (append
   (debug-label 'cg-read-address)
   (cg-mexpr addr)
   (asm 'MLOAD)))

(: cg-read-address-offset  (Generator2 MExpr MExpr))
(define (cg-read-address-offset addr os)
  (append
   (debug-label 'cg-read-address-offset)
   (cg-intros (list addr os))  ; [ addr; os ]
   (cg-reverse 2)              ; [ os; addr ]
   (cg-mul (const WORD) stack) ; [ os'; addr ]
   (cg-add stack stack)        ; [ addr' ]
   (cg-read-address stack)))   ; [ val ]

(: cg-write-address        (Generator2 MExpr MExpr))
(define (cg-write-address dest val)
  (append
   (debug-label 'cg-write-address)
   (cg-intros (list dest val))
   (asm 'MSTORE)))

(: cg-write-address-offset (Generator3 MExpr MExpr MExpr))
(define (cg-write-address-offset dest os val)
  (append
   (debug-label 'cg-write-address-offset)
   (cg-intros (list dest os val))   ; [ addr; os; val ]
   (cg-reverse 2)                   ; [ os; addr; val ]
   (cg-mul (const WORD) stack)      ; [ os'; addr; val ]
   (cg-add stack stack)             ; [ addr'; val ]
   (cg-write-address stack stack))) ; [ ]

(: symbol->integer (-> Symbol Integer)) ; TODO: I think the "official" ABI uses a Keccak hash for this.
(define (symbol->integer sym)
  (let ((lst (string->list (symbol->string sym))))
    (: loop (-> (Listof Char) Integer Integer))
    (define (loop lst i)
      (if (null? lst)
          i
          (loop (cdr lst)
                (+ (char->integer (car lst))
                   (* 256 i)))))
    (loop lst 0)))

(: integer->string (-> Integer String))
(define (integer->string n)
  (: integer->char-list (-> Integer (Listof Char)))
  (define (integer->char-list n)
    (if (equal? n 0)
        null
        (let-values ([ (q r) (quotient/remainder n 256) ])
          (cons (integer->char r)
                (integer->char-list q)))))
  (list->string (reverse (integer->char-list n))))
    
(: asm                     (-> Symbol EthInstructions))
(define (asm sym) (list (eth-asm sym)))

;;; Lists
(: cg-null?        (Generator  MExpr))
(define (cg-null? exp)
  (append
   (debug-label 'cg-null?)
   (cg-tag exp)
   (cg-eq? (const TAG-NIL) stack)))

(: cg-pair?        (Generator  MExpr))
(define (cg-pair? exp)
  (append
   (debug-label 'cg-pair?)
   (cg-tag exp)
   (cg-eq? (const TAG-PAIR) stack)))

(: cg-car          (Generator  MExpr))
(define (cg-car exp)
  (append
   (debug-label 'cg-car)
   (cg-read-address-offset exp (const 1))))

(: cg-cdr          (Generator  MExpr))
(define (cg-cdr exp)
  (append
   (debug-label 'cg-cdr)
   (cg-read-address-offset exp (const 2))))

;; (: cg-list->stack  (Generator  MExpr))
;; (define (cg-list->stack exp)
;;   (append
;;    (debug-label 'cg-list->stack)
;;    (cg-list->vector exp)
;;    (cg-vector->stack stack)))   

; PSEUDOCODE
; (define (list->vector list)
;   (let ((len (list-length list))
;        ((vec (make-vector len '())))
;      (define (loop i list)
;        (if (eq? i len)
;          vec
;          (begin
;            (vector-write vec i (car list))
;            (loop (+ i 1) (cdr list)))))
;      (loop 0 list)))
(: cg-list->vector (Generator  MExpr))
(define (cg-list->vector exp)
  (let ((loop (make-label 'loop))
        (term (make-label 'term)))
    (append
     (debug-label 'cg-list->vector)
     (cg-mexpr exp)                      ; [ list ]
     ; 1. Calculate the length of the list
     (asm 'DUP1)                         ; [ list; list ]
     (cg-list-length stack)              ; [ len; list ]
     (asm 'DUP1)                         ; [ len; len; list ]
     ; 2. Allocate a vector of that size
     (cg-make-vector stack '())          ; [ vector; len; list ]
     (cg-swap 1)                         ; [ len; vector; list ]
     (asm 'DUP2)                         ; [ vector; len; vector; list ]
     (cg-vector-set-length! stack stack) ; [ vector; list ]
     ; 3. Loop through the list, setting vector elements.
     `(,(eth-push 1 0))                  ; [ i; vector; list ]
     ; STACK:                            [ i; vector; list ]
     `(,loop)
     ; 4. Check if loop should be terminated
     (asm 'SWAP2)                        ; [ list; vector; i ]
     (asm 'DUP1)                         ; [ list; list; vector; i]
     (cg-null? stack)                    ; [ term?; list; vector; i ]
     (cg-branch term stack)              ; [ list; vector; i ]
     (asm 'SWAP2)                        ; [ i; vector; list ]
     ; 5. Set vector element, and repeat loop.
     (asm 'DUP1)                         ; [ i; i; vector; list ]
     (asm 'DUP4)                         ; [ list; i; i; vector; list ]
     (cg-car stack)                      ; [ x; i; i; vector; list ]
     (asm 'SWAP1)                        ; [ i; x; i; vector; list ]
     (asm 'DUP4)                         ; [ vector; i; x; i; vector; list ]
     (cg-vector-write stack stack stack) ; [ i; vector; list ]
     (cg-add stack (const 1))            ; [ i'; vector; list ]
     (asm 'SWAP2)                        ; [ list; vector; i' ]
     (cg-cdr stack)                      ; [ list'; vector; i' ]
     (asm 'SWAP2)                        ; [ i'; vector; list' ]
     (cg-goto loop)
     ; STACK:                              [ list; vector; i ]
     `(,term)
     (cg-swap 1)                         ; [ vector; list; i ]
     (cg-swap 2)                         ; [ i; list; vector ]
     (cg-pop 2))))                       ; [ vector ]

(: cg-set-car!     (Generator2 MExpr MExpr))
(define (cg-set-car! addr val)
  (append
   (debug-label 'cg-set-car!)
   (cg-write-address-offset addr (const 1) val)))

(: cg-set-cdr!     (Generator2 MExpr MExpr))
(define (cg-set-cdr! addr val)
  (append
   (debug-label 'cg-set-cdr!)
   (cg-write-address-offset addr (const 2) val)))

; PSEUDOCODE
#|
(define (list-length list)
  (define (loop len)
    (if (pair? list)
        (loop (+ 1 len))
        len))
  (loop 0))
|#
(: cg-list-length  (Generator  MExpr))
(define (cg-list-length exp)
  (let ((loop      (make-label 'loop))
        (terminate (make-label 'terminate)))
    (append
     (debug-label 'cg-list-length) ; [ list ]
     (cg-mexpr exp)           ; [ list ]
     `(,(eth-push 1 0))       ; [ len; list ]
     (cg-swap 1)              ; [ list; len ]
     `(,loop)
     (asm 'DUP1)              ; [ list; list; len ]
     (cg-pair? exp)           ; [ pair?; list; len ]
     (asm 'ISZERO)            ; [ !pair?; list; len ]
     (cg-branch terminate stack) ; [ list; len ]
     (asm 'SWAP1)             ; [ len; list ]
     (cg-add stack (const 1)) ; [ len'; ]
     (asm 'SWAP1)             ; [ list; len' ]
     (cg-cdr stack)           ; [ list'; len' ]
     (cg-goto loop)           ; [ list'; len' ]
     ; STACK                    [ list; len ]
     `(,terminate)
     (cg-pop 1))))            ; [ len ]

(: cg-unroll-args1 (Generator  MExpr))
(define (cg-unroll-args1 exp)
  (append
   (debug-label 'cg-unroll-args1)
   (cg-unroll-list 1 exp)   ; [ x1; nil ]
   (cg-swap 1)              ; [ nil; x1 ]
   (cg-pop 1)               ; [ x1 ]
   (cg-unbox-integer stack) ; [ x1' ]
   ))

(: cg-unroll-args2 (Generator  MExpr))
(define (cg-unroll-args2 exp)
  (append
   (debug-label 'cg-unroll-args2)
   (cg-unroll-list 2 exp)   ; [ x1; x2; nil ]
   (cg-unbox-integer stack) ; [ x1'; x2; nil ]
   (cg-swap 2)              ; [ nil; x2; x1' ]
   (cg-pop 1)               ; [ x2; x1']
   (cg-unbox-integer stack) ; [ x2'; x1' ]
   (cg-swap 1)              ; [ x1'; x2' ]
   ))

(: cg-unroll-args3 (Generator  MExpr))
(define (cg-unroll-args3 exp)
  (append
   (debug-label 'cg-unroll-args3)
   (cg-unroll-list 3 exp)   ; [ x1; x2; x3; nil ]
   (cg-unbox-integer stack) ; [ x1'; x2; x3; nil ]
   (cg-swap 3)              ; [ nil; x2; x3; x1' ]
   (cg-pop 1)               ; [ x2; x3; x1' ]
   (cg-unbox-integer stack) ; [ x2'; x3; x1' ]
   (cg-swap 1)              ; [ x3; x2'; x1' ]
   (cg-unbox-integer stack) ; [ x3'; x2'; x1' ]
   (cg-swap 2)              ; [ x1'; x2'; x3' ]
   ))


;;; Vectors

; PSEUDOCODE

;; (: cg-vector-copy (Generator2 MExpr MExpr))
;; (define (cg-vector-copy ptr len)
;;   (let ([ loop (make-label 'loop)])
;;     (append
;;      (debug-label 'cg-vector-copy) ; [ ]
;;      (cg-intros (list ptr len))    ; [ src; len ]
;;      (asm 'DUP2)                   ; [ len; src; len ]
;;      (cg-allocate stack)           ; [ vec; src; len ]
     
;;      )))

#|
(define (vector->stack vec)
  (let ((i (vector-len vec)))
    (define (loop i)
      (if (eq? i 0)
          (void)
          (begin
            (push (vector-read vec (i-1)))
            (loop i))))
    (loop i)))
|#
;; (: cg-vector->stack (Generator MExpr))
;; (define (cg-vector->stack vec)
;;   (let ((loop (make-label 'loop))
;;         (term (make-label 'term)))
;;     (append
;;      (debug-label 'cg-vector->stack)
;;      (cg-mexpr vec)               ; [ vec ]
;;      (asm 'DUP1)                  ; [ vec; vec ]
;;      (cg-vector-len stack)        ; [ i; vec ]
;;      `(,loop)
;;      (asm 'DUP1)                  ; [ i; i; vec ]
;;      (cg-eq? stack (const 0))     ; [ eq?; i; vec ]
;;      (cg-branch term stack)       ; [ i; vec ]
;;      (asm 'DUP1)                  ; [ i; i; vec ]
;;      (cg-sub stack (const 1))     ; [ i'; i; vec ]
;;      (asm 'DUP3)                  ; [ vec; i'; i; vec ]
;;      (cg-vector-read stack stack) ; [ x; i; vec ]
;;      (asm 'SWAP2)                 ; [ vec; i; x ]
;;      (asm 'SWAP1)                 ; [ i; vec; x ]
;;      (cg-goto loop)               ; [ i; vec; x ]
;;      `(,term)
;;      (cg-pop 2))))                ; [ xs ]

(: cg-vector-data (Generator MExpr))
(define (cg-vector-data vec)
  (cg-add vec (const (* 2 WORD))))

(: cg-vector-len (Generator MExpr))
(define (cg-vector-len vec)
  (append
   (debug-label 'cg-vector-len)
   (cg-read-address-offset vec (const 1))))

(: cg-vector-write (Generator3 MExpr MExpr MExpr)) ; vector -> offset -> value
(define (cg-vector-write vec os val)
  (append
   (debug-label 'cg-vector-write)
   (cg-intros (list vec os val))
   (asm 'SWAP1)             ; [ os; vec; val ]
   (cg-add (const 2) stack) ; [ os'; vec; val ]
   (asm 'SWAP1)             ; [ vec; os'; val ]
   (cg-write-address-offset stack stack stack))) ; [ ]

(: cg-vector-read (Generator2 MExpr MExpr)) ; vector -> offset
(define (cg-vector-read vec os)
  (append
   (debug-label 'cg-vector-read)
   (cg-intros (list vec os))
   (asm 'SWAP1)                  ; [ os; vec ]
   (cg-add (const 2) stack)      ; [ os'; vec ]
   (asm 'SWAP1)                  ; [ vec; os' ]
   (cg-read-address-offset stack stack))) ; [ x ]

(: cg-vector-set-length! (Generator2 MExpr MExpr)) ; vector -> value
(define (cg-vector-set-length! vec len)
  (append
   (debug-label 'cg-vector-set-length!)
   (cg-intros (list vec (const 1) len))        ; [ vec; 1; len ]
   (cg-write-address-offset stack stack stack) ; [ ]
   ))

; PSEUDOCODE
#|
(define (vector-unbox vec)
  (let loop (len (vector-size vec))
    (unless (= len 0)
      (let (i (- len 1))
        (write-vector vec i (unbox-integer (read-vector vec i)))
        (loop i)))))
|#
(: cg-vector-unbox! (Generator MExpr))
(define (cg-vector-unbox! vec)
  (let ([ loop (make-label 'cg-vector-unbox-loop) ]
        [ term (make-label 'cg-vector-unbox-term) ]
        )
  (append
   (debug-label 'cg-vector-unbox)
   (cg-intros (list vec))        ; [ vec ]
   (asm 'DUP1)                   ; [ vec; vec ]
   (cg-vector-len stack)         ; [ len; vec ]
   `(,loop)                      
   (asm 'DUP1)                   ; [ len; len; vec ]
   (cg-eq? (const 0) stack)      ; [ 1; len; vec ]
   (cg-branch term stack)        ; [ len; vec ]
   (cg-sub stack (const 1))      ; [ i; vec ]
   (asm 'DUP1)                   ; [ i; i; vec ]
   (asm 'DUP3)                   ; [ vec; i; i; vec ]
   (cg-vector-read stack stack)  ; [ x; i; vec ]
   (cg-unbox-integer stack)      ; [ x'; i; vec ]
   (asm 'DUP3)                   ; [ vec; x'; i; vec ]
   (asm 'DUP3)                   ; [ i; vec; x'; i; vec ]
   (cg-swap 1)                   ; [ vec; i; x'; i; vec ]
   (cg-vector-write stack stack stack) ; [ i; vec ]
   (cg-goto loop)
   `(,term)                      ; [ len; vec ]
   (cg-pop 1)                    ; [ vec ]
   )))

; PSEUDOCODE
;; (define (add-binding-to-frame! var val frame)
;;  (set-car! frame (cons var (car frame)))
;;  (set-cdr! frame (cons val (cdr frame))))
(: cg-add-binding-to-frame     (Generator3 MExpr MExpr MExpr))
(define (cg-add-binding-to-frame name value frame)
  (append
   (debug-label 'cg-add-binding-to-frame)
   (cg-intros (list name value frame)) ; [ name; value ; frame ]
   (asm 'DUP3)               ; [ frame; name; value; frame ]
   (cg-car stack)            ; [ vars; name; value; frame ]
   (asm 'SWAP1)              ; [ name; vars; value; frame ]
   (cg-cons stack stack )    ; [ vars'; value; frame ]
   (asm 'DUP3)               ; [ frame; vars'; value; frame ]
   (cg-set-car! stack stack) ; [ value; frame ]
   (asm 'DUP2)               ; [ frame; value; frame ]
   (cg-cdr stack)            ; [ vals; value; frame ]
   (asm 'SWAP1)              ; [ value; vals; frame ]
   (cg-cons stack stack )    ; [ vals'; frame ]
   (asm 'SWAP1)              ; [ frame; vals' ]
   (cg-set-cdr! stack stack) ; [ ]
   ))   

;;; Runtime support

(: cg-allocate                 (Generator MExpr))              ; Returns a pointer to a newly-allocated block of 256-bit words.

(: cg-tag                      (Generator MExpr))
(define (cg-tag exp) (append (cg-read-address exp)))

(: cg-make-fixnum              (Generator  MExpr))
(define (cg-make-fixnum val)
  (append
   (debug-label 'cg-make-fixnum)
   (cg-allocate-initialize (const 2) (list (const TAG-FIXNUM) val))))

(: cg-make-symbol              (Generator  MExpr))
(define (cg-make-symbol sym)
  (append
   (debug-label 'cg-make-symbol)
   (cg-allocate-initialize (const 2) (list (const TAG-SYMBOL) sym))))

(: cg-make-compiled-procedure  (Generator2 MExpr MExpr))
(define (cg-make-compiled-procedure code env)
  (append
   (debug-label 'cg-make-compiled-procedure)
   (cg-allocate-initialize (const 3) (list (const TAG-COMPILED-PROCEDURE) code env))))

(: cg-make-primitive-procedure (Generator  MExpr))
(define (cg-make-primitive-procedure code)
  (append
   (debug-label 'cg-make-primitive-procedure)
   (cg-allocate-initialize (const 2) (list (const TAG-PRIMITIVE-PROCEDURE) code))))

(: cg-make-pair                (Generator2 MExpr MExpr))
(define (cg-make-pair fst snd)
  (append
   (debug-label 'cg-make-pair)
   (cg-allocate-initialize (const 3) (list (const TAG-PAIR) fst snd))))

(: cg-cons         (Generator2 MExpr MExpr))
(define cg-cons cg-make-pair)

(: cg-make-vector              (Generator2 MExpr MExprs))
(define (cg-make-vector capacity exps)
  (append
   (debug-label 'cg-make-vector)
   (cg-mexpr capacity)
   (asm 'DUP1)
   (cg-add stack (const 3))
   (cg-allocate-initialize stack
                           (append (list (const TAG-VECTOR)
                                         stack
                                         (const (length exps)))
                                   exps))))

(: cg-make-nil Generator0)
(define (cg-make-nil) (list (eth-push 1 MEM-NIL)))
;  (cg-allocate-initialize (const 1) (list (const TAG-NIL))))

; NOTE: It is currently only valid to call cg-make-list on non-stack items.
; Currently only used to create constant lists, so box the items as we make them.
(: cg-make-list (Generator2 MExprs Boolean))
(define (cg-make-list lst [box? #t])
  (: loop (Generator Integer))
  (define (loop n)
    (if (eq? n 0) ; [ lst; *vals ]
        '()
        (append                   ; [ lst; *vals ]
         (cg-swap 1)              ; [ val'; lst; *vals ]
         (if box?
             (cg-make-fixnum stack)   ; [ val''; lst; *vals ]
             '())
         (cg-cons stack stack)    ; [ 'lst; *vals ]
         (loop (- n 1))         
         ))) 
  (append
   (debug-label 'cg-make-list)
   (cg-intros lst) ; [ *vals ]
   (cg-make-nil)             ; [ nil; *vals ]
   (loop (length lst))))

(define (cg-allocate size)
  (append
   (debug-label 'cg-allocate)
   (cg-mexpr size)                                ; [ size ]
   (cg-mul (const WORD) stack)                    ; [ size']
   (cg-read-address (const MEM-ALLOCATOR))        ; [ ptr; size' ]
   (asm 'DUP1)                                    ; [ ptr; ptr; size' ]
   (asm 'SWAP2)                                   ; [ size'; ptr; ptr ]
   (cg-add stack stack)                           ; [ ptr'; ptr ]
   (cg-write-address (const MEM-ALLOCATOR) stack) ; [ ptr ]
   ))

(: cg-allocate-initialize      (Generator2 MExpr MExprs))      ; Allocates memory and initializes each word from the argument list.
(define (cg-allocate-initialize size inits)
  (: loop (Generator MExprs))
  (define (loop exps)
    (if (null? exps)
        '()
        (append
         ; STACK                          [ ptr; optr; exp* ]
         (asm 'SWAP2)                   ; [ exp; optr; ptr; exp* ]
         (asm 'DUP3)                    ; [ ptr; exp; optr; ptr; exp* ]
         (cg-write-address stack stack) ; [ optr; ptr; exp* ]
         (cg-reverse 2)                 ; [ ptr; optr; exp* ]
         (cg-add (const WORD) stack)    ; [ ptr'; optr; exp* ]
         (loop (cdr exps)))))
  (append
   (debug-label 'cg-allocate-initialize)
   (cg-intros (cons size inits))        ; [ size; exp* ]
   (cg-allocate stack)                  ; [ optr; exp* ]
   (asm 'DUP1)                          ; [ optr; optr; exp* ]
   (loop inits)                         ; [ ptr; optr ]
   (cg-pop 1)                           ; [ optr ]
   ))

(: cg-initialize-program Generator0)
(define (cg-initialize-program)
  (append
   (debug-label 'cg-initialize-program)
   ; Set the dynamic memory allocator pointer
   (list (eth-push 2 MEM-DYNAMIC-START))
   (list (eth-push 1 MEM-ALLOCATOR))
   (cg-write-address stack stack)
   ; Initialize an empty environment
   (cg-make-environment)
   (cg-write-address (const MEM-ENV) stack)
   ; Initialize the memory-backed registers
   (cg-write-address (const MEM-CONTINUE) (const 31337))
   (cg-write-address (const MEM-PROC)     (const 1337))
   (cg-write-address (const MEM-ARGL)     (const 337))
   ; Initialize global constants
   (cg-write-address (const MEM-NIL) (const TAG-NIL))
   ))

(: cg-define-primops Generator0)
(define (cg-define-primops)
  (let ((label-skip (make-label 'cg-define-primops-skip)))
    (append
     (debug-label 'cg-install-standard-library)
     (cg-goto label-skip)
     ; Primops
     ; op-lookup-variable-value
     `(,*label-op-lookup-variable-value*)      ; [ var; env; ret ]
     (cg-op-lookup-variable-value stack stack) ; [ result; ret ]
     (asm 'SWAP1)                              ; [ ret; result ]
     (cg-goto stack)                           ; [ result ]
     ; op-define-variable!
     `(,*label-op-define-variable!*)            ; [ var; val; env; ret ]
     (cg-op-define-variable! stack stack stack) ; [ ret ]
     (cg-goto stack)                            ; [ ]
     ; op-set-variable!
     `(,*label-op-set-variable-value!*)            ; [ var; val; env; ret ]
     (cg-op-set-variable-value! stack stack stack) ; [ ret ]
     (cg-goto stack)                               ; [ ]
     ; op-return
     `(,*label-op-return*) ; [ val ]
     (cg-return stack)     ; [ ]
     ; op-restore-continuation
     `(,*label-op-restore-continuation*) ; [ cont ]
     (cg-restore-continuation stack)     ; [ ]
     ; op-primitive-procedure?
     `(,*label-op-primitive-procedure?*) ; [ proc; ret ]
     (cg-op-primitive-procedure? stack)  ; [ proc?; ret ]
     (asm 'SWAP1)                        ; [ ret; proc? ]
     (cg-goto stack)                     ; [ proc? ]
     ; op-compiled-procedure?
     `(,*label-op-compiled-procedure?*)  ; [ proc; ret ]
     (cg-op-compiled-procedure? stack)   ; [ proc?; ret ]
     (asm 'SWAP1)                        ; [ ret; proc? ]
     (cg-goto stack)                     ; [ proc? ]
     ; op-continuation?
     `(,*label-op-continuation?*)         ; [ proc; ret ]
     (cg-op-continuation? stack)         ; [ proc?; ret ]
     (asm 'SWAP1)                        ; [ ret; proc? ]
     (cg-goto stack)                     ; [ proc? ]
     ; Install the primitives
     `(,label-skip)
     )))
                                  

(: cg-make-environment Generator0)
(define (cg-make-environment)
  (append
   (debug-label 'cg-make-environment)
   (cg-make-nil)   ; Enclosing environment
   (cg-make-frame) ; Empty frame
   (cg-cons stack stack)
   ))

(: cg-make-frame Generator0)
(define (cg-make-frame)
  (append
   (debug-label 'cg-make-frame)
   (cg-make-nil)
   (cg-make-nil)
   (cg-cons stack stack)))

(: cg-return (Generator MExpr))              ; Ends the program with a boxed value as the output.
(define (cg-return exp)
  (let ([ label-uint256 (make-label 'cg-return-ty-uint256-)]
        [ label-list    (make-label 'cg-return-ty-list)]
        [ label-vector  (make-label 'cg-return-ty-vector)]
        [ label-compiled-procedure (make-label 'cg-return-ty-compiled-procedure)]
        [ label-nil     (make-label 'cg-return-ty-nil)]
        )
    (append
     (debug-label 'cg-return)
     (cg-intros (list exp))     ; [ exp ]
     (asm 'DUP1)                ; [ exp; exp ]
     (cg-tag stack)             ; [ tag; exp ]
     
     (asm 'DUP1)                       ; [ tag; tag; exp ]
     (cg-eq? (const TAG-FIXNUM) stack) ; [ pred; tag; exp ]
     (cg-branch label-uint256 stack)   ; [ tag; exp ]
     
     (asm 'DUP1)                     ; [ tag; tag; exp ]
     (cg-eq? (const TAG-PAIR) stack) ;  [ pred; tag; exp ]
     (cg-branch label-list stack)    ; [ tag; exp ]
     
     (asm 'DUP1)                       ; [ tag; tag; exp ]
     (cg-eq? (const TAG-VECTOR) stack) ; [ pred; tag; exp ]
     (cg-branch label-vector stack)    ; [ tag; exp ]

     (asm 'DUP1) ; [ tag; tag; exp ]
     (cg-eq? (const TAG-COMPILED-PROCEDURE) stack) ; [ pred; tag; exp ]
     (cg-branch label-compiled-procedure stack)    ; [ tag; exp ]

     (asm 'DUP1) ; [ tag; tag; exp ]
     (cg-eq? (const TAG-NIL) stack) ; [ pred; tag; exp ]
     (cg-branch label-nil stack)
     
     (cg-throw 'cg-return-invalid-type)

     `(,label-compiled-procedure)   ; [ tag; exp ]
     (cg-pop 1)                     ; [ exp ]
     (cg-return-compiled-procedure stack)
     `(,label-vector)           ; [ tag; exp ]
     (cg-pop 1)                 ; [ exp ]
     (cg-return-vector stack)
     `(,label-list)             ; [ tag; exp ]
     (cg-pop 1)                 ; [ exp ]
     (cg-return-list-uint256 stack)
     `(,label-uint256)          ; [ tag; exp ]
     (cg-pop 1)                 ; [ exp ]
     (cg-return-uint256 stack)
     `(,label-nil)
     (asm 'STOP)
     )))

(: cg-return-uint256 (Generator MExpr))
(define (cg-return-uint256 exp)
  (append
   (debug-label 'cg-return-uint256) ; [ n ]
   (cg-add (const WORD) exp)        ; [ &val ]
   `(,(eth-push 1 WORD))            ; [ 1; &val ]
   (cg-reverse 2)                   ; [ &val; 1 ]
   (asm 'RETURN)))                  ; [ ]

(: cg-return-vector (Generator MExpr))
(define (cg-return-vector exp)
  (append
   (debug-label 'cg-return-vector) ; [ vec ]
   (asm 'DUP1)                     ; [ vec; vec ]
   (cg-vector-len stack)           ; [ len; vec ]
   (cg-mul (const WORD) stack)     ; [ len'; vec ]
   (cg-reverse 2)                  ; [ vec; len ]
   (cg-vector-data stack)          ; [ data; len ]
   (asm 'RETURN)
   ))

(: cg-return-list-uint256 (Generator MExpr))
(define (cg-return-list-uint256 exp)
  (append
   (debug-label 'cg-return-list-uint256)
   (cg-list->vector exp)  ; [ vec ]
   (cg-vector-unbox! stack) ; [ vec' ]
   (cg-return-vector stack) ; [ ]
   ))
  
(define cg-return-compiled-procedure cg-return-uint256)

; Leaves [ x1; x2; ... ; xn; remaining list ] on the stack.
(: cg-unroll-list (Generator2 Integer MExpr))
(define (cg-unroll-list num exp)
  (: loop (Generator Integer))
  (define (loop n)
    (if (eq? n 0)
        '()
        (append
         (asm 'DUP1)
         (cg-car stack)
         (asm 'SWAP1)
         (cg-cdr stack)
         (loop (- n 1)))))
  (append
   (debug-label 'cg-unroll-list)
   (cg-mexpr exp)
   (loop num)
   (cg-reverse (+ num 1)))) ; +1 because of trailing NIL or tail

;;; Continuations

(: cg-save-continuation Generator0)
(define (cg-save-continuation)
  (append
   (debug-label 'cg-save-continuation)                       ; [ ]
   (cg-allocate (const 4))                                   ; [ ptr; ]
   (asm 'DUP1)                                               ; [ ptr; ptr; ]
   (cg-write-address stack (const TAG-CONTINUATION))         ; [ ptr; ]
   (asm 'DUP1)                                               ; [ ptr; ptr ]
   (cg-write-address-offset stack (const 1) (reg 'continue)) ; [ ptr ]
   (asm 'DUP1)                                               ; [ ptr; ptr ]
   (cg-write-address-offset stack (const 2) (reg 'env))      ; [ ptr ]
   ))
        
(: cg-restore-continuation (Generator MExpr))
(define (cg-restore-continuation cont)
  (append
   (debug-label 'cg-restore-continuation)    ; [ ]
   (cg-intros (list cont))                   ; [ cont ]
   (asm 'DUP1)                               ; [ cont; cont ]
   (cg-read-address-offset stack (const 2))  ; [ env; cont ]
   (cg-write-reg (reg 'env) stack)           ; [ cont ]
   (cg-read-address-offset stack (const 1))  ; [ code ]
   (cg-goto stack)
   ))

;;; Arithmetic

; The arity of nullop, unop, binop refer to the number of values popped from the stack.
; So, cg-eq has a net stack impact of 2 pops

(: cg-unbox-integer (Generator MExpr))
(define (cg-unbox-integer exp)
  (append
   (debug-label 'cg-unbox-integer)
   (cg-read-address-offset exp (const 1))))

(: cg-eq? (Generator2 MExpr MExpr))
(define (cg-eq? a b)
  (append
   (debug-label 'cg-eq?)
   (cg-intros (list a b))
   (asm 'EQ)))

(: cg-mul (Generator2 MExpr MExpr))
(define (cg-mul a b)
  (append
   (debug-label 'cg-mul)
   (cg-intros (list a b))
   (asm 'MUL)))

(: cg-add (Generator2 MExpr MExpr))
(define (cg-add a b)
  (append
   (debug-label 'cg-add)
   (cg-intros (list a b))
   (asm 'ADD)))

(: cg-sub (Generator2 MExpr MExpr))
(define (cg-sub a b)
  (append
   (debug-label 'cg-sub)
   (cg-intros (list a b))
   (asm 'SUB)))

; Record that a label is located at a specific Ethereum bytecode offset.
;; (: mark-label (-> LabelName Address Void))
;; (define (mark-label name addr)
;;   (push-mlist! label-map (cons name addr)))

#| DOCUMENTED FUNCTIONS
The functions below were non-obvious enough that I wrote documentation & examples. They are separated
from most other functions to not break the flow when reading.

; (: cg-shuffle (Generator (Listof Fixnum)))
-- Shuffle Algorithm --
cg-shuffle is used to rearrange the top few stack elements to an arbitrary order.

We want to emit a sequence of top-level stack swaps that changes [1,2,3,4] to [3,4,1,2]
1. Locate the element that should be relocated to the deepest spot.
2. Swap it to the front, or do nothing if it's already there.
3. Swap it to the back.
4. Recurse onto the (n-1) length stack, possibly with a different desired ordering.

Example:
[1,2,3,4] -> 2 is the deepest element of [3,4,1,2]
[2,1,3,4] -> swap 2 to the front
[4,1,3,2] -> swap 2 to the back
Rename [4,1,3] to [1,2,3] in the desired solution, and iterate:
(shuffle [ 2,3,1 ])
|#

#|

There are two calling conventions: "Normal" Scheme dynamic memory, and the stack-based primitive operations.
The stack-based ones allow constants and other primitive operations to be passed as arguments.
Arguments passed on the stack are no-ops, while constants and primitives need to have code emitted.
This function emits the code to get all arguments onto the stack first-to-last.

PSEUDOCODE:
* Traverse the arguments first-to-last, with i = 0 .. n
* If an element writes to the stack, insert it into position i.
* If it was already on the stack, do nothing.
(intros '()) = '()
(intros (snoc xs x)) = [ if (stack-write? x) (cg-insert x (length xs)) '(), intros xs ]

Example:
exprs = [ stk, stk, stk, 5, stk, 10 ]
Stack: [ x1, x2, x3, x4 ]
10 writes to stack, so insert it at 4: [ x1, x2, x3, x4, 10 ]
Iterate on [ stk, stk, stk, 5, stk ]
stk is a no-op, so immediately recurse: [ stk, stk, stk, 5 ]
5 inserted at 3: [ x1,x2,x3,5,x4,10]
Remaining 3 stk arguments ignored. 
|#
(: cg-intros               (Generator MExprs)) ; Use at start of a primitive op. Ensures all arguments are on the stack first-to-last.
(define (cg-intros exprs)
  (: loop (Generator2 MExprs Integer))
  (define (loop exprs n)
    (if (null? exprs)
        '()
        (let ((x (car exprs))
              (xs (cdr exprs)))
          (append
           (cond ((stack-read? x)
                  (append
                   (debug-label 'cg-intros-stack-read)
                   (cg-swap n)
                   (cg-mexpr x)
                   (cg-swap n)))
                 ((stack-write? x)
                  (append
                   (debug-label 'cg-intros-stack-write)
                   (cg-insert x n)))
                 (else
                  (debug-label 'cg-intros-null)))
           (loop xs (+ n 1))))))
  (loop exprs 0))

#|
1. Evaluate the expression so it's on the front: [ c; x1; x2; x3; ]
2. Emit N swaps. For (cg-insert c 3):
SWAP3 -> [ x3; x1; x2; c ]
SWAP2 -> [ x2; x1; x3; c ]
SWAP1 -> [ x1; x2; x3; c ]
|#
(: cg-insert (Generator2 MExpr Integer)) ;
(define (cg-insert expr pos)
  (: loop (Generator Integer))
  (define (loop n)
    (cond ((eq? n 0) '())
          ;((eq? n 1) '())
          (else (append (cg-swap n)
                        (loop (- n 1))))))
  (append (cg-mexpr expr)
          (loop pos)))

(: cg-invoke-primop (-> MExpr MExpr * EthInstructions))
(define (cg-invoke-primop target . args)
  (let ([ ret (make-label 'cg-invoke-primop-ret) ])
    (append
     (cg-intros (append (list target) args (list ret))) ; [ target; args*; ret ]
     (cg-goto stack)                                    ; [ args*; ret ]
     `(,ret)                                            ; [ ]
     )))

; (: cg-move-front (Generator Fixnum))

; The only expressions that don't affect the stack are reads from the stack-aliased registers.
(: stack-write?            (-> MExpr Boolean))
(define (stack-write? exp) (not (stack? exp)))

(define (stack-read? exp)
  (if (op? exp)
      (if (list? (op-args exp))
          (memf stack? (op-args exp))
          (stack? (op-args exp)))
      false))

; Debug labels are not used for flow control. They generate entries in the relocation table that
; help locate the code that generated an assembly fragment. They have 1 byte of overhead for a
; JUMPDEST instruction, but even that could be eliminated if necessary.
(: debug-label (Generator Symbol))
(define (debug-label sym)
  (if (*use-debug-symbols?*)
      (list (make-label sym))
      '()))

(: cg-throw (Generator Symbol))
(define (cg-throw sym)
  (append (debug-label sym)
          (asm 'REVERT)))
