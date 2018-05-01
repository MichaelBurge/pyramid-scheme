#lang typed/racket

(require typed/racket/unsafe)

(require (submod "types.rkt" abstract-machine))
(require (submod "types.rkt" evm-assembly))
(require "types.rkt")
(require "utils.rkt")
(require "ast.rkt")
(require "globals.rkt")
(require racket/list)

(require (submod "typed.rkt" binaryio))

(provide ;codegen
 ;codegen-list
 (all-defined-out)
 (all-from-out 'constants)
 (all-from-out (submod "types.rkt" evm-assembly)))

#|
-- Registers -> Stack --
The abstract machine uses 6 256-bit virtual registers
* env:        &(0 * WORD)
* proc:       &(1 * WORD)
* continue:   &(2 * WORD)
* argl:       &(3 * WORD)
* val:        &(4 * WORD)
* stack-size: &(5 * WORD)

Addresses have 8-bit granularity, so we multiply by WORD so they don't overlap.
Arguments to primitive procedures are passed on the stack. The first argument is the first stack entry.
Arguments to compiled procedures are passed in a linked list through the argl register.

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
 * 6: Null:                0 words     - Only the tag is used.
 * 7: Continuation:        3 words     - continue register, env register, and stack vector
 * 8: Frame:               2 words     - Pointer to names, pointer to values
 * 9: Environment:         2 words     - Pointer to frame, pointer to enclosing environment
 * 10: Character           1 word      - The Unicode codepoint for the character
 * 11: Bytes               1 + ceil(n/WORD) words - A size n, followed by enough words to hold n bytes

Additionally, there are derived objects used in the standard library:
 * List:        Null | (pair X List)

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
* Continuations
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


(module constants typed/racket
  (provide (all-defined-out))

  (define TAG-FIXNUM              0)
  (define TAG-SYMBOL              1)
  (define TAG-COMPILED-PROCEDURE  2)
  (define TAG-PRIMITIVE-PROCEDURE 3)
  (define TAG-PAIR                4)
  (define TAG-VECTOR              5)
  (define TAG-NIL                 6)
  (define TAG-CONTINUATION        7)
  (define TAG-FRAME               8)
  (define TAG-ENVIRONMENT         9)
  (define TAG-CHARACTER           10)
  (define TAG-BYTES               11)

  (define MEM-ENV           #x00)
  (define MEM-PROC          #x20)
  (define MEM-CONTINUE      #x40)
  (define MEM-ARGL          #x60)
  (define MEM-VAL           #x80)
  (define MEM-STACK-SIZE    #xa0)
  (define MEM-NIL           #xc0)
  (define MEM-ALLOCATOR     #xe0)
  (define MEM-DYNAMIC-START #x100) ; This should be the highest hardcoded memory address.
  )

(require 'constants)

(define-syntax (define-generator stx)
  (syntax-case stx ()
    [(_ (name args ... . rest) body ...)
     #'(define (name args ... . rest)
         (define sym (quote name))
         (parameterize ([ *evm-source-map-stack* (cons sym (*evm-source-map-stack*))])
           (let ([ label-source (make-label sym 0 #t)])
             (hash-set! (*evm-source-map*) (label-name label-source) (*evm-source-map-stack*))
             (append
              (list label-source)
              body ...))))]))

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
  (cond ((label?   i) (cg-label   i))
        ((symbol?  i) (error "Unexpected symbol - codegen-one" i))
        ((assign?  i) (cg-assign  i))
        ((test?    i) (cg-test    i))
        ((branch?  i) (cg-branch (branch-dest i) stack))
        ((goto?    i) (cg-goto (goto-dest i)))
        ((save?    i) (cg-save    i))
        ((restore? i) (cg-restore i))
        ((perform? i) (cg-perform i))
        ((evm?     i) (evm-insts  i))
        (else
         (error "Unknown instruction type -- codegen-one:" i))))

(: cg-label (Generator label-definition))
(define (cg-label i) (list i))

(: cg-assign  (Generator assign))
(define-generator (cg-assign i)
  (let ((value : MExpr (assign-value i))
        (target : RegisterName (assign-reg-name i)))
     (cg-write-reg (reg target) value)))

(: cg-test    (Generator test))
(define-generator (cg-test i)
  (cg-mexpr (test-condition i)))

(: cg-save    (Generator save))
(define-generator (cg-save i)
  (cg-write-stack (save-exp i)))

(: cg-restore (Generator restore))
(define-generator (cg-restore i)
  (cg-write-reg (reg (restore-reg-name i)) stack))

(: cg-perform (Generator perform))
(define-generator (cg-perform i)
  (cg-mexpr (perform-action i))) ; TODO: Watch the number of stack writes here to pop them.

(: reg-address (-> reg Natural))
(define (reg-address r)
  (register-address (reg-name r)))

(: register-address (-> RegisterName Natural))
(define (register-address r)
  (match r
    ['env        MEM-ENV]
    ['proc       MEM-PROC]
    ['continue   MEM-CONTINUE]
    ['argl       MEM-ARGL]
    ['val        MEM-VAL]
    ['stack-size MEM-STACK-SIZE]
    [x (error "reg-address: Unknown register" x)]))

;;; Primitive operations emitted by the abstract compiler

(: op-lookup-variable-value (Generator2 MExpr MExpr))
(define (op-lookup-variable-value name env)     (cg-invoke-primop *label-op-lookup-variable-value* name env))

(: op-define-variable! (Generator3 MExpr MExpr MExpr))
(define (op-define-variable! name value env)    (cg-invoke-primop *label-op-define-variable!* name value env))

(: op-set-variable-value! (Generator3 MExpr MExpr MExpr))
(define (op-set-variable-value! name value env) (cg-invoke-primop *label-op-set-variable-value!* name value env))

(: op-return (Generator MExpr))
(define (op-return value)                       (cg-jump-primop *label-op-return* value))

(: op-restore-continuation (Generator MExpr))
(define (op-restore-continuation cont)          (cg-jump-primop *label-op-restore-continuation* cont))

(: op-compiled-procedure? (Generator MExpr))
(define (op-compiled-procedure? obj)            (cg-invoke-primop *label-op-compiled-procedure?* obj))

(: op-continuation? (Generator MExpr))
(define (op-continuation? obj)                  (cg-invoke-primop *label-op-continuation?* obj))

(: op-primitive-procedure? (Generator MExpr))
(define (op-primitive-procedure? obj)           (cg-invoke-primop *label-op-primitive-procedure?* obj))

(: cg-op-box (Generator  MExpr))       ; Creates a boxed integer or symbol.
(define-generator (cg-op-box exp)
  (if (const? exp)
      (cond ((integer? (const-value exp)) (cg-make-fixnum exp))
            ((symbol?  (const-value exp)) (cg-make-symbol exp))
            (else
             (error "cg-op-box: Unsupported boxed constant" exp)))
      (error "cg-op-box: Can only box constants" exp)))

(: cg-op-extend-environment (Generator3 MExpr MExpr MExpr)) ; Adds a frame to the environment.
(define-generator (cg-op-extend-environment vars vals env)
  (cg-intros (list vars vals env))   ; [ vars; vals; env ]
  (cg-make-frame stack stack)        ; [ frame; env ]
  (cg-make-environment stack stack)) ; [ env' ]

(: cg-op-make-compiled-procedure (Generator2 MExpr MExpr)) ; A lambda creates a runtime value from a code pointer & closure.
(define-generator (cg-op-make-compiled-procedure code env)
  (cg-make-compiled-procedure code env))

(: cg-op-compiled-procedure-entry (Generator  MExpr))       ; The code pointer from a make-compiled-procedure object.
(define-generator (cg-op-compiled-procedure-entry obj)
  (cg-read-address-offset obj (const 1)))

(: cg-op-compiled-procedure-env (Generator  MExpr))       ; The environment address from a make-compiled-procedure object.
(define-generator (cg-op-compiled-procedure-env obj)
  (cg-read-address-offset obj (const 2)))

(: cg-op-compiled-procedure? (Generator MExpr))
(define-generator (cg-op-compiled-procedure? obj)
  (cg-tag obj)                                   ; [ tag ]
  (cg-eq? (const TAG-COMPILED-PROCEDURE) stack)) ; [ eq? ]

(: cg-op-continuation? (Generator  MExpr))
(define-generator (cg-op-continuation? obj)
  (cg-tag obj)
  (cg-eq? (const TAG-CONTINUATION) stack))

(: cg-op-primitive-procedure? (Generator  MExpr))       ; Is the object at an address a primitive procedure?
(define-generator (cg-op-primitive-procedure? obj)
  (cg-tag obj)
  (cg-eq? (const TAG-PRIMITIVE-PROCEDURE) stack))

(: cg-op-primitive-procedure-entry (Generator MExpr))
(define-generator (cg-op-primitive-procedure-entry obj)
  (cg-read-address-offset obj (const 1)))

(: cg-op-apply-primitive-procedure (Generator2 MExpr MExpr)) ; Calls the primitive procedure with a code pointer and argument list.
(define-generator (cg-op-apply-primitive-procedure proc argl)
  (let ((after-op (make-label 'after-op)))
    (append
     (cg-intros (list proc argl after-op))
     ; Arguments passed as a dynamic list.
     (cg-op-primitive-procedure-entry stack) ; [ primop-entry; args; return ]
     (asm 'JUMP)
     `(,after-op))))

(: cg-op-cons (Generator2 MExpr MExpr))
(define-generator (cg-op-cons a b)
  (cg-cons a b))

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
(define-generator (cg-op-lookup-variable-value name env)
  (let ((env-loop    (make-label 'lookup-variable-value-env-loop))
        (scan        (make-label 'lookup-variable-value-scan))
        (scan-else-1 (make-label 'lookup-variable-value-scan-else-1))
        (scan-else-2 (make-label 'lookup-variable-value-scan-else-2))
        (term        (make-label 'lookup-variable-value-term))
        (not-found   (make-label 'lookup-variable-value-not-found))
        )
    (append
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
(define-generator (cg-op-set-variable-value! name value env)
  (let ([env-loop    (make-label 'set-variable-value-env-loop)]
        [scan        (make-label 'set-variable-value-scan)]
        [scan-else-1 (make-label 'set-variable-value-scan-else-1)]
        [scan-else-2 (make-label 'set-variable-value-scan-else-2)]
        [term        (make-label 'set-variable-value-term)]
        [not-found   (make-label 'set-variable-value-not-found)]
        )
    (append
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
(define-generator (cg-op-define-variable! name value env)
  (let ((scan        (make-label 'define-variable!-scan))
        (scan-else-1 (make-label 'define-variable!-scan-else-1))
        (scan-else-2 (make-label 'define-variable!-scan-else-2))
        (term        (make-label 'define-variable!-term))
        )
    (append
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

(: cg-op-false? (Generator MExpr))       ; Evaluates to 1 if the expression is 0
(define-generator (cg-op-false? exp)
  (cg-mexpr exp)
  (asm 'ISZERO))

(: cg-op-list (Generator MExpr))       ; Creates a 1-element vector with the given object.
(define-generator (cg-op-list exp)
  (cg-mexpr exp)         ; [ x ]
  (cg-make-nil)          ; [ nil; x ]
  (cg-reverse 2)         ; [ x; nil ]
  (cg-cons stack stack)) ; [ x:nil]


(: cg-mexpr (Generator MExpr))
(define-generator (cg-mexpr exp)
  (cond ((reg?   exp)  (cg-mexpr-reg   exp))
        ((const? exp)  (cg-mexpr-const exp))
        ((boxed-const? exp) (cg-mexpr-boxed-const exp))
        ((op?    exp)  (cg-mexpr-op    exp))
        ((label? exp)  (cg-mexpr-label exp))
        ((stack? exp)  '())
        ((evm?   exp)  (append (debug-label 'cg-mexpr-evm)
                               (evm-insts exp)))
        (else
         (error "cg-mexpr: Unknown mexpr" exp (list? exp)))))

(: cg-mexpr-reg (Generator reg))
(define-generator (cg-mexpr-reg dest)
  (cg-read-address (const (reg-address dest))))

(: cg-write-reg (Generator2 reg MExpr))
(define-generator (cg-write-reg dest exp)
  (cg-write-address (const (reg-address dest)) exp))

(: cg-mexpr-const (Generator const))
(define-generator (cg-mexpr-const exp)
  (let ((val (const-value exp)))
    (cond [(symbol? val)
           (let ((int (symbol->integer val)))
             (list (evm-push (integer-bytes int) int)))]
          [(nonnegative-integer? val) (list (evm-push (integer-bytes val) val))]
          [(negative-integer? val) (let ([ word-val (truncate-int (+ WORDLIMIT val))])
                                     (list (evm-push (integer-bytes word-val) word-val)))]
          [(list?    val) (cg-make-list (map const val) #f)]
          [(boolean? val) (cg-mexpr-const (const (if val 1 0)))]
          [(vector?  val) (cg-make-vector (const (vector-length val)) (map const (vector->list val)))]
          [(char?    val) (list (evm-push 'shrink (char->integer val)))]
          [(string?  val) (cg-make-bytes-literal (string->bytes/utf-8 val) #f)]
          [else           (error "cg-mexpr-const: Unsupported constant" exp)])))


(: cg-mexpr-boxed-const (Generator boxed-const))
(define-generator (cg-mexpr-boxed-const exp)
  (let ([ val (boxed-const-value exp) ])
    (cond [(symbol? val)  (cg-make-symbol (const (symbol->integer val)))]
          [(integer? val) (cg-make-fixnum (const val))]
          [(list? val)    (cg-make-list (map const val) #t)]
          [(vector? val)  (cg-make-vector (const (vector-length val)) (map boxed-const (vector->list val)))]
          [(char? val)    (cg-make-char (const val))]
          [(string? val)  (cg-make-bytes-literal (string->bytes/utf-8 val) #t)]
          [else           (error "cg-mexpr-boxed-const: Unsupported constant" exp)])))

(unsafe-require/typed "unsafe.rkt"
                      [ unsafe-apply (-> Procedure (Listof Any) EthInstructions)])

(: cg-mexpr-op (Generator op))
(define-generator (cg-mexpr-op exp)
  (let* ([ name (op-name exp)]
         [ args (op-args exp)]
         [ tbl (*primops*)]
         [ primop (find-primop tbl name)]
         [ proc (primop-gen primop)]
         )
    (unless (procedure-arity-includes? proc (length args))
      (error "cg-mexpr-op: Attempted to invoke primop with incorrect number of arguments" name (procedure-arity proc) args))
    (unsafe-apply proc args)))

(: find-primop (-> PrimopTable Symbol primop))
(define (find-primop tbl name)
  (if (hash-has-key? tbl name)
      (hash-ref tbl name)
      (error "find-op: Unknown primop" name)))

(: cg-mexpr-label (Generator (U label-definition label)))
(define-generator (cg-mexpr-label exp)
  (list (evm-push 'shrink exp)))

(: cg-mexpr-stack Generator0)
(define-generator (cg-mexpr-stack) '())

(: cg-write-stack (Generator MExpr))
(define-generator (cg-write-stack exp) (cg-mexpr exp))

(: cg-goto (Generator  MExpr))
(define-generator (cg-goto dest)
  (cg-mexpr dest)
  (asm 'JUMP))

(: cg-branch (Generator2 MExpr MExpr))
(define-generator (cg-branch dest pred)
  (cg-intros (list dest pred))
  (asm 'JUMPI))

(: cg-reverse (Generator Integer))
(define-generator (cg-reverse size)
  (match size
    [ 2 (asm 'SWAP1)]
    [ 3               ; [ x1; x2; x3 ]
        (asm 'SWAP2)] ; [ x3; x2; x1 ]
    [ 4 (append       ; [ x1; x2; x3; x4 ]
         (asm 'SWAP3) ; [ x4; x2; x3; x1 ]
         (asm 'SWAP1) ; [ x2; x4; x3; x1 ]
         (asm 'SWAP2) ; [ x3; x4; x2; x1 ]
         (asm 'SWAP1) ; [ x4; x3; x2; x1 ]
         )]
    [ _ (error "Unsupported size -- cg-reverse" size)]))

(: cg-pop (Generator (U Integer MExpr)))
(define-generator (cg-pop size)
  (if (integer? size)
      (for/list : EthInstructions ([ i (in-range size)])
        (evm-op 'POP))
      (cgm-repeat size
                  (append
                   (asm 'SWAP1)
                   (asm 'POP)))))

(: cg-swap (Generator  Integer))
(define-generator (cg-swap size)
  (match size
    [ 1 (asm 'SWAP1)]
    [ 2 (asm 'SWAP2)]
    [ 3 (asm 'SWAP3)]
    [ 4 (asm 'SWAP4)]
    [ 5 (asm 'SWAP5)]
    [ 6 (asm 'SWAP6)]
    [ 7 (asm 'SWAP7)]
    [ 8 (asm 'SWAP8)]
    [ 9 (asm 'SWAP9)]
    [10 (asm 'SWAP10)]
    [11 (asm 'SWAP11)]
    [12 (asm 'SWAP12)]
    [13 (asm 'SWAP13)]
    [14 (asm 'SWAP14)]
    [15 (asm 'SWAP15)]
    [0  '()]
    (else (error "Unknown swap size -- cg-swap" size))))

(: cg-read-address (Generator  MExpr))
(define-generator (cg-read-address addr)
  (cg-mexpr addr)
  (asm 'MLOAD))

(: cg-read-address-offset (Generator2 MExpr MExpr))
(define-generator (cg-read-address-offset addr os)
  (cg-intros (list addr os))  ; [ addr; os ]
  (cg-reverse 2)              ; [ os; addr ]
  (cg-mul (const WORD) stack) ; [ os'; addr ]
  (cg-add stack stack)        ; [ addr' ]
  (cg-read-address stack))    ; [ val ]

(: cg-write-address (Generator2 MExpr MExpr))
(define-generator (cg-write-address dest val)
  (cg-intros (list dest val))
  (asm 'MSTORE))

(: cg-write-address-offset (Generator3 MExpr MExpr MExpr))
(define-generator (cg-write-address-offset dest os val)
  (cg-intros (list dest os val))  ; [ addr; os; val ]
  (cg-reverse 2)                  ; [ os; addr; val ]
  (cg-mul (const WORD) stack)     ; [ os'; addr; val ]
  (cg-add stack stack)            ; [ addr'; val ]
  (cg-write-address stack stack)) ; [ ]

(: asm (-> Symbol EthInstructions))
(define (asm sym) (list (evm-op sym)))

;;; Lists
(: cg-null? (Generator  MExpr))
(define-generator (cg-null? exp)
  (cg-tag exp)
  (cg-eq? (const TAG-NIL) stack))

(: cg-pair? (Generator  MExpr))
(define-generator (cg-pair? exp)
  (cg-tag exp)
  (cg-eq? (const TAG-PAIR) stack))

(: cg-car (Generator  MExpr))
(define-generator (cg-car exp)
  (cg-read-address-offset exp (const 1)))

(: cg-cdr (Generator  MExpr))
(define-generator (cg-cdr exp)
  (cg-read-address-offset exp (const 2)))

(: cgm-repeat (Generator2 MExpr EthInstructions))
(define-generator (cgm-repeat size body)
  (match size
    [(const (? exact-integer? x))
     (apply append (for/list : (Listof EthInstructions) ([ i (in-range x)])
                     body))]
    [_ (let ([ label-loop (make-label 'cgm-repeat-loop)]
             [ label-term (make-label 'cgm-repeat-term)])
         (append
          (cg-mexpr size)              ; [ size ]
          `(,label-loop)               ; [ size ]
          (asm 'DUP1)                  ; [ size; size ]
          (asm 'ISZERO)                ; [ 0?; size ]
          (cg-branch label-term stack) ; [ size; ]
          body                         ; [ size ]
          (cg-sub stack (const 1))     ; [ size' ]
          (cg-goto label-loop)         ; [ size' ]
          `(,label-term)               ;
          (asm 'POP)                   ; [ ]
          ))]))

(: cg-pop-vector (Generator MExpr))
(define-generator (cg-pop-vector size)
  (cg-mexpr size)              ; [ size; STACK* ]
  (asm 'DUP1)                  ; [ size; size; STACK* ]
  (cg-make-vector stack '())   ; [ ptr; size; STACK* ]
  (cg-add stack (const WORD))  ; [ ptr+1; size; STACK* ]
  (asm 'SWAP1)                 ; [ size; ptr+1; STACK* ]
  (cgm-repeat stack
              (append       ; [ size; ptr+1; STACK* ]
               (asm 'SWAP2) ; [ x; ptr+1; size; STACK'* ]
               (asm 'DUP3)  ; [ size; x; ptr+1; size; STACK'* ]
               (asm 'DUP3)  ; [ ptr+1; size; x; ptr+1; size; STACK'* ]
               (cg-write-address-offset stack stack stack) ; [ ptr+1; size; STACK'* ]
               (asm 'SWAP1) ; [ size; ptr+1; STACK'* ]
               ))           ; [ ptr+1 ]
  (cg-sub stack (const WORD)) ; [ ptr ]
  )


(: cg-push-vector (Generator MExpr))
(define-generator (cg-push-vector vec)
  (cg-mexpr vec)                ; [ vec ]
  (asm 'DUP1)                   ; [ vec; vec ]
  (cg-vector-len stack)         ; [ len; vec ]
  (asm 'DUP1)                   ; [ orig-len; len; vec ]
  (asm 'SWAP2)                  ; [ vec; len; orig-len ]
  (cg-add stack (const WORD))   ; [ vec+1; len; orig-len]
  (asm 'SWAP1)                  ; [ len; vec+1; orig-len]
  (cgm-repeat stack
              (append          ; [ len; vec; orig-len ]
               (asm 'DUP1)     ; [ len; len; vec; orig-len ]
               (asm 'DUP4)     ; [ orig-len; len; len; vec; orig-len ]
               (asm 'SUB)      ; [ os; len; vec; orig-len ]
               (cg-add stack (const 1)) ; [ os'; len; vec; orig-len ]
               (asm 'DUP3)     ; [ vec; os; len; vec; orig-len ]
               (cg-read-address-offset stack stack) ; [ x; len; vec; orig-len ]
               (asm 'SWAP3)    ; [ orig-len; len; vec; x ]
               (asm 'SWAP2)    ; [ vec; len; orig-len; x ]
               (asm 'SWAP1)    ; [ len; vec; orig-len; x ]
               ))              ; [ vec; orig-len; STACK* ]
  (cg-pop 2))                  ; [ STACK* ]

(: cg-unbox-vector! (Generator MExpr))
(define-generator (cg-unbox-vector! vec)
  (cg-mexpr vec)               ; [ vec ]
  (asm 'DUP1)                  ; [ vec; vec ]
  (cg-vector-length stack)     ; [ len; vec ]
  (cgm-repeat stack
              (append
               (asm 'DUP1)     ; [ len; len; vec ]
               (cg-add (const 1) stack); [os; len; vec ]
               (asm 'DUP1)     ; [ os; os; len; vec ]
               (asm 'DUP4)     ; [ vec; os; os; len; vec ]
               (cg-read-address-offset stack stack) ; [ x; os; len; vec ]
               (cg-fixnum-value stack) ; [ x'; os; len; vec ]
               (asm 'SWAP1)    ; [ os; x'; len; vec ]
               (asm 'DUP4)     ; [ vec; os; x'; len; vec ]
               (cg-write-address-offset stack stack stack) ; [ len; vec ]
               )))

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
(define-generator (cg-list->vector exp)
  (let ((loop (make-label 'loop))
        (term (make-label 'term)))
    (append
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
     `(,(evm-push 1 0))                  ; [ i; vector; list ]
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
     (cg-pop 2)))) ; [ vector ]

;; ; PSEUDOCODE
;; ; (define (list->vector list)
;; ;   (let ((len (list-length list))
;; ;        ((vec (make-vector len '())))
;; ;      (define (loop i list)
;; ;        (if (eq? i len)
;; ;          vec
;; ;          (begin
;; ;            (vector-write vec i (car list))
;; ;            (loop (+ i 1) (cdr list)))))
;; ;      (loop 0 list)))
;; (: cg-list->vector (Generator  MExpr))
;; (define-generator (cg-list->vector exp)
;;   (let ((loop (make-label 'loop))
;;         (term (make-label 'term)))
;;     (append
;;      (cg-mexpr exp)                      ; [ list ]
;;      ; 1. Calculate the length of the list
;;      (asm 'DUP1)                         ; [ list; list ]
;;      (cg-list-length stack)              ; [ len; list ]
;;      (asm 'DUP1)                         ; [ len; len; list ]
;;      ; 2. Allocate a vector of that size
;;      (cg-make-vector stack '())          ; [ vector; len; list ]
;;      (cg-swap 1)                         ; [ len; vector; list ]
;;      (asm 'DUP1)                         ; [ len; len; vector; list ]
;;      (asm 'DUP3)                         ; [ vector; len; len; vector; list ]
;;      (cg-vector-set-length! stack stack) ; [ len; vector; list ]

;;      ; 3. Loop through the list, setting vector elements.
;;      `(,(evm-push 1 0))                  ; [ i; len; vector; list ]
;;      ; STACK:                            [ i; len; vector; list ]
;;      `(,loop)
;;      ; 4. Check if loop should be terminated
;;      (asm 'SWAP3)                        ; [ list; len; vector; i ]
;;      (asm 'DUP1)                         ; [ list; list; len; vector; i]
;;      (cg-null? stack)                    ; [ term?; list; len; vector; i ]
;;      (cg-branch term stack)              ; [ list; len; vector; i ]
;;      (asm 'SWAP3)                        ; [ i; len; vector; list ]
;;      ; 5. Set vector element, and repeat loop.
;;      (asm 'DUP1)                         ; [ i; i; len; vector; list ]
;;      (asm 'DUP5)                         ; [ list; i; i; len; vector; list ]
;;      (cg-car stack)                      ; [ x; i; i; len; vector; list ]
;;      (asm 'SWAP1)                        ; [ i; x; i; len; vector; list ]
;;      (asm 'DUP4)                         ; [ len; i; x; i; len; vector; list ]
;;      (asm 'SUB)                          ; [ i'; x; i; len; vector; list ]
;;      (asm 'DUP5)                         ; [ vector; i'; x; i; len; vector; list ]
;;      (cg-vector-write stack stack stack) ; [ i; len; vector; list ]
;;      (cg-add stack (const 1))            ; [ i'; len; vector; list ]
;;      (asm 'SWAP2)                        ; [ len; vector; i' ]
;;      (cg-cdr stack)                      ; [ list'; vector; i' ]
;;      (asm 'SWAP2)                        ; [ i'; vector; list' ]
;;      (cg-goto loop)
;;      ; STACK:                              [ list; vector; i ]
;;      `(,term)
;;      (cg-swap 1)                         ; [ vector; list; i ]
;;      (cg-swap 2)                         ; [ i; list; vector ]
;;      (cg-pop 2))))                       ; [ vector ]

(: cg-set-car! (Generator2 MExpr MExpr))
(define-generator (cg-set-car! addr val)
  (cg-write-address-offset addr (const 1) val))

(: cg-set-cdr! (Generator2 MExpr MExpr))
(define-generator (cg-set-cdr! addr val)
  (cg-write-address-offset addr (const 2) val))

; PSEUDOCODE
#|
(define (list-length list)
  (define (loop len)
    (if (pair? list)
        (loop (+ 1 len))
        len))
  (loop 0))
|#
(: cg-list-length (Generator MExpr))
(define-generator (cg-list-length exp)
  (let ((loop      (make-label 'loop))
        (terminate (make-label 'terminate)))
    (append
     (cg-mexpr exp)           ; [ list ]
     `(,(evm-push 1 0))       ; [ len; list ]
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

;;; Vectors

(: cg-vector-data (Generator MExpr))
(define-generator (cg-vector-data vec)
  (cg-add vec (const (* 2 WORD))))

(: cg-vector-len (Generator MExpr))
(define-generator (cg-vector-len vec)
  (cg-read-address-offset vec (const 1)))

(define cg-vector-length cg-vector-len)

(: cg-vector-write (Generator3 MExpr MExpr MExpr)) ; vector -> offset -> value
(define-generator (cg-vector-write vec os val)
  (cg-intros (list vec os val))
  (asm 'SWAP1)             ; [ os; vec; val ]
  (cg-add (const 2) stack) ; [ os'; vec; val ]
  (asm 'SWAP1)             ; [ vec; os'; val ]
  (cg-write-address-offset stack stack stack)) ; [ ]

(: cg-vector-read (Generator2 MExpr MExpr)) ; vector -> offset
(define-generator (cg-vector-read vec os)
  (cg-intros (list vec os))
  (asm 'SWAP1)                  ; [ os; vec ]
  (cg-add (const 2) stack)      ; [ os'; vec ]
  (asm 'SWAP1)                  ; [ vec; os' ]
  (cg-read-address-offset stack stack)) ; [ x ]

(: cg-vector-set-length! (Generator2 MExpr MExpr)) ; vector -> value
(define-generator (cg-vector-set-length! vec len)
  (cg-intros (list vec (const 1) len))        ; [ vec; 1; len ]
  (cg-write-address-offset stack stack stack) ; [ ]
  )

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
(define-generator (cg-vector-unbox! vec)
  (let ([ loop (make-label 'cg-vector-unbox-loop) ]
        [ term (make-label 'cg-vector-unbox-term) ]
        )
  (append
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
(: cg-add-binding-to-frame (Generator3 MExpr MExpr MExpr))
(define-generator (cg-add-binding-to-frame name value frame)
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
  )

;;; Runtime support

(: cg-tag (Generator MExpr))
(define-generator (cg-tag exp) (append (cg-read-address exp)))

(: cg-make-fixnum (Generator  MExpr))
(define-generator (cg-make-fixnum val)
  (cg-allocate-initialize (const 2) (list (const TAG-FIXNUM) val)))

(: cg-make-symbol (Generator  MExpr))
(define-generator (cg-make-symbol sym)
  (cg-allocate-initialize (const 2) (list (const TAG-SYMBOL) sym)))

(: cg-make-compiled-procedure (Generator2 MExpr MExpr))
(define-generator (cg-make-compiled-procedure code env)
  (cg-allocate-initialize (const 3) (list (const TAG-COMPILED-PROCEDURE) code env)))

(: cg-make-primitive-procedure (Generator  MExpr))
(define-generator (cg-make-primitive-procedure code)
  (cg-allocate-initialize (const 2) (list (const TAG-PRIMITIVE-PROCEDURE) code)))

(: cg-make-pair (Generator2 MExpr MExpr))
(define-generator (cg-make-pair fst snd)
  (cg-allocate-initialize (const 3) (list (const TAG-PAIR) fst snd)))

(: cg-make-frame (Generator2 MExpr MExpr))
(define-generator (cg-make-frame fst snd)
  (cg-allocate-initialize (const 3) (list (const TAG-FRAME) fst snd)))

(: cg-make-environment (Generator2 MExpr MExpr))
(define-generator (cg-make-environment fst snd)
  (cg-allocate-initialize (const 3) (list (const TAG-ENVIRONMENT) fst snd)))

(: cg-cons (Generator2 MExpr MExpr))
(define cg-cons cg-make-pair)

(: cg-make-vector (Generator2 MExpr MExprs))
(define-generator (cg-make-vector size exps)
  (cg-mexpr size)
  (asm 'DUP1)
  (cg-add stack (const 2))
  (cg-allocate-initialize stack
                          (append (list (const TAG-VECTOR)
                                        stack)
                                  exps)))

(: cg-make-nil Generator0)
(define-generator (cg-make-nil) (list (evm-push 1 MEM-NIL)))
;  (cg-allocate-initialize (const 1) (list (const TAG-NIL))))

; NOTE: It is currently only valid to call cg-make-list on non-stack items.
; Currently only used to create constant lists, so box the items as we make them.
(: cg-make-list (Generator2 MExprs Boolean))
(define-generator (cg-make-list lst [box? #t])
  (cg-intros (reverse lst)) ; [ *vals ]
  (cg-make-nil)             ; [ nil; *vals ]
  (let loop : EthInstructions ([ n : Integer (length lst)])
    (if (eq? n 0) ; [ lst; *vals ]
        '()
        (append                   ; [ lst; *vals ]
         (cg-swap 1)              ; [ val'; lst; *vals ]
         (if box?
             (cg-make-fixnum stack) ; [ val''; lst; *vals ]
             '())
         (cg-cons stack stack)    ; [ 'lst; *vals ]
         (loop (- n 1))
         ))))

(: cg-allocate (Generator MExpr))              ; Returns a pointer to a newly-allocated block of 256-bit words.
(define-generator (cg-allocate size)
  (cg-mexpr size)                                ; [ size ]
  (cg-mul (const WORD) stack)                    ; [ size']
  (cg-read-address (const MEM-ALLOCATOR))        ; [ ptr; size' ]
  (asm 'DUP1)                                    ; [ ptr; ptr; size' ]
  (asm 'SWAP2)                                   ; [ size'; ptr; ptr ]
  (cg-add stack stack)                           ; [ ptr'; ptr ]
  (cg-write-address (const MEM-ALLOCATOR) stack) ; [ ptr ]
  )

(: cg-allocate-initialize (Generator2 MExpr MExprs))      ; Allocates memory and initializes each word from the argument list.
(define-generator (cg-allocate-initialize size inits)
  (cg-intros (cons size inits))        ; [ size; exp* ]
  (cg-allocate stack)                  ; [ optr; exp* ]
  (asm 'DUP1)                          ; [ optr; optr; exp* ]
  (let loop : EthInstructions ([ exps : MExprs inits])   ; [ ptr; optr ]
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
  (cg-pop 1)                            ; [ optr ]
  )

(: cg-initialize-program Generator0)
(define-generator (cg-initialize-program)
  ; Set the dynamic memory allocator pointer
  (list (evm-push 2 MEM-DYNAMIC-START))
  (list (evm-push 1 MEM-ALLOCATOR))
  (cg-write-address stack stack)
  ; Initialize an empty environment
  (cg-empty-environment)
  (cg-write-address (const MEM-ENV) stack)
  ; Initialize the memory-backed registers
  (cg-write-address (const MEM-CONTINUE) (const 31337))
  (cg-write-address (const MEM-PROC)     (const 1337))
  (cg-write-address (const MEM-ARGL)     (const 337))
  ; Initialize global constants
  (cg-write-address (const MEM-NIL) (const TAG-NIL))
  )

(: cg-define-primops Generator0)
(define-generator (cg-define-primops)
  (let ((label-skip (make-label 'cg-define-primops-skip)))
    (append
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


(: cg-empty-environment Generator0)
(define-generator (cg-empty-environment)
  (cg-make-nil)
  (cg-empty-frame)
  (cg-make-environment stack stack)
  )

(: cg-empty-frame Generator0)
(define-generator (cg-empty-frame)
  (cg-make-nil)
  (cg-make-nil)
  (cg-make-frame stack stack)
  )

(: cg-return (Generator MExpr))              ; Ends the program with a boxed value as the output.
(define-generator (cg-return exp)
  (let ([ label-uint256 (make-label 'cg-return-ty-uint256-)]
        [ label-list    (make-label 'cg-return-ty-list)]
        [ label-vector  (make-label 'cg-return-ty-vector)]
        [ label-compiled-procedure (make-label 'cg-return-ty-compiled-procedure)]
        [ label-nil     (make-label 'cg-return-ty-nil)]
        [ label-unboxed (make-label 'cg-return-ty-unboxed)]
        [ label-bytes   (make-label 'cg-return-ty-bytes)]
        )
    (append
     (cg-intros (list exp))     ; [ exp ]

     ; Check for unboxed values first
     (asm 'DUP1)                ; [ exp; exp ]
     (cg-gt? (const 2) stack)   ; [ bool?; exp ]
     (cg-branch label-unboxed stack) ; [ exp ]

     ; Otherwise the value is boxed, so branch on the type
     (asm 'DUP1)
     (cg-tag stack)             ; [ tag; exp ]

     (asm 'DUP1)                       ; [ tag; tag; exp ]
     (cg-eq? (const TAG-FIXNUM) stack) ; [ pred; tag; exp ]
     (cg-branch label-uint256 stack)   ; [ tag; exp ]

     (asm 'DUP1)                          ; [ tag; tag; exp ]
     (cg-eq? (const TAG-CHARACTER) stack) ; [ pred; tag; exp ]
     (cg-branch label-uint256 stack)      ; [ tag; exp ]

     (asm 'DUP1)                     ; [ tag; tag; exp ]
     (cg-eq? (const TAG-PAIR) stack) ;  [ pred; tag; exp ]
     (cg-branch label-list stack)    ; [ tag; exp ]

     (asm 'DUP1)                       ; [ tag; tag; exp ]
     (cg-eq? (const TAG-SYMBOL) stack) ; [ pred; tag; exp ]
     (cg-branch label-uint256 stack)   ; [ tag; exp ]

     (asm 'DUP1)                       ; [ tag; tag; exp ]
     (cg-eq? (const TAG-VECTOR) stack) ; [ pred; tag; exp ]
     (cg-branch label-vector stack)    ; [ tag; exp ]

     (asm 'DUP1) ; [ tag; tag; exp ]
     (cg-eq? (const TAG-COMPILED-PROCEDURE) stack) ; [ pred; tag; exp ]
     (cg-branch label-compiled-procedure stack)    ; [ tag; exp ]

     (asm 'DUP1) ; [ tag; tag; exp ]
     (cg-eq? (const TAG-NIL) stack) ; [ pred; tag; exp ]
     (cg-branch label-nil stack)

     (asm 'DUP1) ; [ tag; tag; exp ]
     (cg-eq? (const TAG-BYTES) stack) ; [ pred; tag; exp ]
     (cg-branch label-bytes stack)

     (cg-throw 'cg-return-invalid-type)

     `(,label-unboxed)              ; [ exp ]
     (cg-make-fixnum stack)         ; [ exp' ]
     (asm 'DUP1)                    ; [ exp'; exp' ]
     (cg-goto label-uint256)

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
     `(,label-bytes)
     (cg-pop 1)                 ; [ exp ]
     (cg-return-bytes stack)
     )))

(: cg-return-uint256 (Generator MExpr))
(define-generator (cg-return-uint256 exp)
  (cg-add (const WORD) exp)        ; [ &val ]
  `(,(evm-push 1 WORD))            ; [ 1; &val ]
  (cg-reverse 2)                   ; [ &val; 1 ]
  (asm 'RETURN)                    ; [ ]
  )

(: cg-return-vector (Generator MExpr))
(define-generator (cg-return-vector exp)
  (asm 'DUP1)                 ; [ vec; vec ]
  (cg-vector-len stack)       ; [ len; vec ]
  (cg-mul (const WORD) stack) ; [ len'; vec ]
  (cg-reverse 2)              ; [ vec; len ]
  (cg-vector-unbox! stack)    ; [ vec'; len ]
  (cg-vector-data stack)      ; [ data; len ]
  (asm 'RETURN)
  )

(: cg-return-list-uint256 (Generator MExpr))
(define-generator (cg-return-list-uint256 exp)
  (cg-list->vector exp)  ; [ vec ]
  (cg-return-vector stack) ; [ ]
  )

(: cg-return-bytes (Generator MExpr))
(define-generator (cg-return-bytes exp)
  (cg-mexpr exp) ; [ bs ]
  (asm 'DUP1) ; [ bs; bs ]
  (cg-bytes-len stack) ; [ len ; bs]
  (cg-swap 1) ; [ bs; len ]
  (cg-bytes-data stack) ; [ data; len ]
  (asm 'RETURN)
  )

(define cg-return-compiled-procedure cg-return-uint256)

; Leaves [ x1; x2; ... ; xn; remaining list ] on the stack.
(: cg-unroll-list (Generator2 Integer MExpr))
(define-generator (cg-unroll-list num exp)
  (cg-mexpr exp)                  ; [ lst ]
  (let loop : EthInstructions ([ n : Integer num ])
    (if (eq? n 0)
        '()
        (append                   ; [ lst ]
         (asm 'DUP1)              ; [ lst; lst ]
         (cg-car stack)           ; [ x; lst ]
         (asm 'SWAP1)             ; [ lst; x ]
         (cg-cdr stack)           ; [ xs; x ]
         (loop (- n 1)))))        ; [ STACK* ]
  (cg-reverse (+ num 1))) ; +1 because of trailing NIL or tail

;;; Continuations

(: cg-save-continuation Generator0)
(define-generator (cg-save-continuation)
  (cg-pop-vector (reg 'stack-size))                         ; [ vec ]
  (cg-allocate (const 4))                                   ; [ ptr; vec]
  (asm 'DUP1)                                               ; [ ptr; ptr; vec]
  (cg-write-address stack (const TAG-CONTINUATION))         ; [ ptr; vec]
  (asm 'DUP1)                                               ; [ ptr; ptr vec]
  (cg-write-address-offset stack (const 1) (reg 'continue)) ; [ ptr; vec ]
  (asm 'DUP1)                                               ; [ ptr; ptr; vec]
  (cg-write-address-offset stack (const 2) (reg 'env))      ; [ ptr; vec ]
  (asm 'DUP1)                                               ; [ ptr; ptr; vec ]
  (asm 'SWAP2)                                              ; [ vec; ptr; ptr ]
  (asm 'SWAP1)                                              ; [ ptr; vec; ptr ]
  (cg-write-address-offset stack (const 3) stack)           ; [ ptr ]
  (cg-write-reg (reg 'stack-size) stack)                    ; [ ]
  (cg-continuation-stack (reg 'stack-size))                 ; [ stack]
  (cg-push-vector stack)                                    ; [ *STACK ]
  (cg-mexpr (reg 'stack-size))                              ; [ ptr ]
  (asm 'DUP1)                                               ; [ ptr; ptr ]
  (cg-continuation-stack-size stack)                        ; [ size; ptr ]
  (cg-write-reg (reg 'stack-size) stack)                    ; [ ptr ]
  )

(: cg-restore-continuation (Generator MExpr))
(define-generator (cg-restore-continuation cont)
  (cg-write-reg (reg 'env) cont)            ; [ ]
  (cg-pop (reg 'stack-size))                ; [ ERASED-STACK ]
  (cg-mexpr (reg 'env))                     ; [ cont ]
  (asm 'DUP1)                               ; [ cont; cont ]
  (asm 'DUP1)                               ; [ cont; cont; cont ]
  (cg-read-address-offset stack (const 2))  ; [ env; cont; cont ]
  (cg-write-reg (reg 'env) stack)           ; [ cont; cont ]
  (cg-read-address-offset stack (const 1))  ; [ code; cont ]
  (cg-write-reg (reg 'continue) stack)      ; [ cont ]
  (cg-continuation-stack stack)             ; [ stack-ptr
  (asm 'DUP1)                               ; [ stack-ptr; stack-ptr]
  (cg-vector-len stack)                     ; [ size; stack-ptr ]
  (cg-write-reg (reg 'stack-size) stack)    ; [ stack-ptr ]
  (cg-push-vector stack)                    ; [ STACK* ]
  (cg-goto (reg 'continue))                 ; [ STACK* ]
  )

(: cg-continuation-stack (Generator MExpr))
(define-generator (cg-continuation-stack cont)
  (cg-read-address-offset cont (const 3)))

(: cg-continuation-stack-size (Generator MExpr))
(define-generator (cg-continuation-stack-size cont)
  (cg-continuation-stack cont) ; [ vec ]
  (cg-vector-len stack)        ; [ size ]
  )

;;; Arithmetic

; The arity of nullop, unop, binop refer to the number of values popped from the stack.
; So, cg-eq has a net stack impact of 2 pops

(: cg-unbox-integer (Generator MExpr))
(define-generator (cg-unbox-integer exp)
  (cg-read-address-offset exp (const 1)))

(define cg-fixnum-value cg-unbox-integer)

(: cg-eq? (Generator2 MExpr MExpr))
(define-generator (cg-eq? a b)
  (cg-intros (list a b))
  (asm 'EQ))

(: cg-lt? (Generator2 MExpr MExpr))
(define-generator (cg-lt? a b)
  (cg-intros (list a b))
  (asm 'LT))

(: cg-gt? (Generator2 MExpr MExpr))
(define-generator (cg-gt? a b)
  (cg-intros (list a b))
  (asm 'GT))

(: cg-mul (Generator2 MExpr MExpr))
(define-generator (cg-mul a b)
  (cg-intros (list a b))
  (asm 'MUL))

(: cg-add (Generator2 MExpr MExpr))
(define-generator (cg-add a b)
  (cg-intros (list a b))
  (asm 'ADD))

(: cg-sub (Generator2 MExpr MExpr))
(define-generator (cg-sub a b)
  (cg-intros (list a b))
  (asm 'SUB))

;;; Characters
(: cg-make-char (Generator MExpr))
(define-generator (cg-make-char x)
  (cg-allocate-initialize (const 2) (list (const TAG-CHARACTER) x)))

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
(: cg-intros (Generator MExprs)) ; Use at start of a primitive op. Ensures all arguments are on the stack first-to-last.
(define-generator (cg-intros exprs)
  (let loop : EthInstructions ([ exprs : MExprs exprs] [ n : Integer 0])
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
           (loop xs (+ n 1)))))))

#|
1. Evaluate the expression so it's on the front: [ c; x1; x2; x3; ]
2. Emit N swaps. For (cg-insert c 3):
SWAP3 -> [ x3; x1; x2; c ]
SWAP2 -> [ x2; x1; x3; c ]
SWAP1 -> [ x1; x2; x3; c ]
|#
(: cg-insert (Generator2 MExpr Integer)) ;
(define-generator (cg-insert expr pos)
  (cg-mexpr expr)
  (let loop : EthInstructions ([ n : Integer pos ])
       (match n
         [0 '()]
         [_ (append (cg-swap n)
                    (loop (- n 1)))])))

(: cg-jump-primop (-> MExpr MExpr * EthInstructions))
(define-generator (cg-jump-primop target . args)
  (cg-intros (cons target args)) ; [ target; args* ]
  (cg-goto stack))               ; [ args* ]

(: cg-invoke-primop (-> MExpr MExpr * EthInstructions))
(define-generator (cg-invoke-primop target . args)
  (let ([ ret (make-label 'cg-invoke-primop-ret) ])
    (append
     (cg-intros (append (list target) args (list ret))) ; [ target; args*; ret ]
     (cg-goto stack)                                    ; [ args*; ret ]
     `(,ret)                                            ; [ ]
     )))

(: cg-identity (Generator1 MExpr))
(define-generator (cg-identity x) (cg-mexpr x))

; (: cg-move-front (Generator Fixnum))

; The only expressions that don't affect the stack are reads from the stack
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
      (list (make-label sym 0 #t))
      '()))

(: cg-throw (Generator Symbol))
(define (cg-throw sym)
  (append (debug-label sym)
          (asm 'REVERT)))

; Fixnums, symbols, and characters are all stored the same way. Use define-generator to generate a sourcemap frame, though.
(: cg-character-value (Generator MExpr))
(define-generator (cg-character-value x)
  (cg-fixnum-value x))

(: cg-symbol-value (Generator MExpr))
(define-generator (cg-symbol-value x)
  (cg-fixnum-value x))

(: cg-codecopy (Generator3 MExpr label MExpr))
(define-generator (cg-codecopy dest-ptr lbl-data num-bytes)
  (cg-intros (list dest-ptr lbl-data num-bytes)) ; [ mem-ptr; code-ptr; size ]
  (asm 'CODECOPY)                                ; [ ]
  )

; TODO: Move literals to the end of the program to avoid generating a JUMP
(: cg-make-bytes-literal (Generator2 Bytes Boolean))
(define-generator (cg-make-bytes-literal bs boxed?)
  (let ([ lbl-data (make-label 'literal 1)]
        [ lbl-after (make-label 'literal-after)]
        [ size-bytes (bytes-length bs)])
    (append (cg-goto lbl-after)
            `(,lbl-data)
            (list (evm-bytes bs))
            `(,lbl-after) ; [ ]
            (if boxed?
                (append (cg-allocate (const (+ 2 size-bytes)))             ; [ mem-ptr ]
                        (asm 'DUP1)                                        ; [ mem-ptr; mem-ptr ]
                        (cg-write-address stack (const TAG-BYTES))         ; [ mem-ptr ]
                        (asm 'DUP1)                                        ; [ mem-ptr; mem-ptr ]
                        (cg-add stack (const WORD))                        ; [ mem-ptr+; mem-ptr]
                        (cg-write-address stack (const size-bytes))        ; [ mem-ptr ]
                        (asm 'DUP1)                                        ; [ mem-ptr; mem-ptr ]
                        (cg-add stack (const (* 2 WORD)))                  ; [ data-ptr; mem-ptr ]
                        (cg-codecopy stack lbl-data (const size-bytes)))   ; [ mem-ptr]
                (append (cg-allocate (const size-bytes))                   ; [ mem-ptr ]
                        (asm 'DUP1)                                        ; [ mem-ptr; mem-ptr ]
                        (cg-codecopy stack lbl-data (const size-bytes)))   ; [ mem-ptr]
            ))))

(: cg-bytes-len (Generator MExpr))
(define-generator (cg-bytes-len bs)
  (cg-add bs (const WORD))
  (cg-read-address stack)
  )

(: cg-bytes-data (Generator MExpr))
(define-generator (cg-bytes-data bs)
  (cg-add bs (const (* 2 WORD))))
