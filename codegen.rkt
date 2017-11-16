#lang typed/racket/no-check

(require "evaluator.rkt")
(require "types.rkt")
(require "interpreter.rkt")

; WORD = 0x20

#|
-- Registers -> Stack --
The abstract machine uses 5 256-bit virtual registers
* env:      &(0 * WORD)
* proc:     STACK
* val:      STACK
* argl:     STACK
* continue: &(1 * WORD)
Addresses have 8-bit granularity, so we multiply by WORD so they don't overlap.
Arguments to procedures are passed on the stack. The first argument is the first stack entry.

-- Runtime Representation --
An object is represented by its memory address pointer.
Objects can be queried for types at runtime, so they must be tagged with a type.
An object's pointer points to a tag. Depending on the tag, additional data follows:
 * 0: Fixnum:              1 word      - The number's value
 * 1: Symbol:              1 word      - 32 8-bit ASCII characters
 * 2: Compiled Procedure:  2 words     - A code pointer, and closure environment pointer
 * 3: Primitive Procedure: 1 word      - A code pointer
 * 4: Pair:                2 words     - Pointer to first element, pointer to second element
 * 5: Vector:              2 + n words - A capacity n, a size, followed by n pointers to individual elements
 * 6: Nil:                 0 words     - Only the tag is used.

Additionally, there are derived objects used in the standard library:
 * Environment: (pair frame enclosing-environment)
 * Frame:       (pair vars  vals)
 * List:        Nil | (pair X List)

-- Assembly --
Some procedures are written in EVM assembly. To help validate them, they are annotated as follows:
* Arguments are first-to-last on the stack.
* The stack or its changes are on each assembly line
** A list with no qualifiers is the current stack: [ x; y; z]
**  +x means that x was pushed to the front of the stack
**  -x means that x was removed from the front of the stack
**  x <-> y means that object x was swapped with object y
* A goto must arrive on a line with the same stack size.
* A branch pops one element, and then arrives on a line with the same stack size.
* The procedure must terminate with a stack size of 0

-- Primitive operations --
There is a standard library of primitive operations.
* Some ops are emitted by the compiler. Others are available for users.
* Primitive operations are always inlined. A later update may instead emit a single copy and save/restore the 'continue register
|#

(define-type Address Fixnum)
(struct asm         ([ name : Symbol ]))
(struct eth-push    ([ size : Fixnum ]) ([ value : Integer ]))
(struct eth-unknown ([ opcode : Fixnum ]))
(define-type EthInstruction     (U asm eth-push eth-unknown label))
(define-type EthInstructions    (Listof   EthInstruction))
(define-type (Generator  A)     (-> A     EthInstructions))
(define-type (Generator2 A B)   (-> A B   EthInstructions))
(define-type (Generator3 A B C) (-> A B C EthInstructions))

; Constants
(define TAG-FIXNUM              0)
(define TAG-SYMBOL              1)
(define TAG-COMPILED-PROCEDURE  2)
(define TAG-PRIMITIVE-PROCEDURE 3)
(define TAG-PAIR                4)
(define TAG-VECTOR              5)
(define TAG-NIL                 6)

(define PRIM-LOOKUP-VARIABLE-VALUE 1)

(define MEM-ENV      #x0)
(define MEM-CONTINUE #x20)
(define MEM-ALLOCATOR #x40)
(define MEM-DYNAMIC-START #x60) ; This should be the highest hardcoded memory address.

; Global variables
(define WORD       #x20) ; 256-bit words / 8 bit granularity addresses
(define byte-index 0)    ; Used to turn labels into code addresses
(define mem-index  0)    ; Used to allocate memory

(: codegen (Generator Instructions))
(define (codegen is)
  (if (null? is)
      '()
      (append (codegen-one (car is))
              (codegen     (cdr is)))))
       

(: codegen-one (Generator Instruction))
(define (codegen-one i)
  (cond ((label?   i) (cg-label   i))
        ((assign?  i) (cg-assign  i))
        ((test?    i) (cg-test    i))
        ((branch?  i) (cg-branch  i))
        ((goto?    i) (cg-goto    i))
        ((save?    i) (cg-save    i))
        ((restore? i) (cg-restore i))
        ((perform? i) (cg-perform i))
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

(define (cg-label i) i)

(define (cg-assign i)
  (let ((value   (assign-value-exp i))
        (target (assign-reg-name i)))
    (append (cg-mexpr value)
            (cg-write (reg target)))))

(define (cg-test i)
  (cg-mexpr (test-condition i)))

(define (cg-branch i)
  (cg-mexpr (branch-dest i))
  (asm 'JUMPI))

(define (cg-goto i)
  (cg-mexpr (goto-dest i))
  (asm 'JUMP))

(define (cg-save i)
  (append (cg-mexpr (reg (stack-inst-reg-name i)))
          (cg-write (reg 'val))))

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
  (cond ((eq? (reg-name dest) 'env)      (cg-read-address (const #x0)))
        ((eq? (reg-name dest) 'continue) (cg-read-address (const #x20)))
        (else                            (cg-mexpr-stack))))

(: cg-write-reg (Generator2 reg MExpr))
(define (cg-write-reg dest exp)
  (cond ((eq? (reg-name dest) 'env)      (cg-write-address (const #x0)  exp))
        ((eq? (reg-name dest) 'continue) (cg-write-address (const #x20) exp))
        (else                            (cg-write-stack))))

(: cg-mexpr-const (Generator const))
(define (cg-mexpr-const val)
  (eth-push 32 val)) ; TODO: Dynamically adjust size based on value

(: cg-mexpr-op    (Generator op))
(define (cg-mexpr-op exp)
  (let ((name (op-name exp))
        (args (op-args exp)))
    (cond ((eq? name 'compiled-procedure-entry)  (cg-compiled-procedure-entry  (car args)))
          ((eq? name 'compiled-procedure-env)    (cg-compiled-procedure-env    (car args)))
          ((eq? name 'primitive-procedure?)      (cg-primitive-procedure?      (car args) (cadr args)))
          ((eq? name 'apply-primitive-procedure) (cg-apply-primitive-procedure (car args) (cadr args)))
          ((eq? name 'lookup-variable-value)     (cg-lookup-variable-value     (car args) (cadr args)))
          ((eq? name 'define-variable!)          (cg-define-variable!          (car args) (cadr args) (caddr args)))
          ((eq? name 'false?)                    (cg-false?                    (car args)))
          ((eq? name 'list)                      (cg-list                      (car args)))
          (else
           (error "Unknown primitive op - cg-mexpr-op" exp)))))
          
(: cg-mexpr-stack (Generator Nothing))
(define cg-mexpr-stack (void))

; Writes the top of the stack to the given destination
(: cg-write (Generator MExpr))
(define (cg-write dest)
  (cond ((reg? exp) (cg-write-reg dest))
        (else
         (error "Can only write to registers - cg-write" dest))))

(: cg-write-stack (Generator Nothing))
(define cg-write-stack (void))
  

;;; Primitive operations emitted by the abstract compiler


(: cg-op-make-compiled-procedure   (Generator2 MExpr MExpr)) ; A lambda creates a runtime value from a code pointer & closure.
(: cg-op-compiled-procedure-entry  (Generator  MExpr))       ; The code pointer from a make-compiled-procedure object.
(: cg-op-compiled-procedure-env)   (Generator  MExpr))       ; The environment address from a make-compiled-procedure object.
(: cg-op-primitive-procedure?      (Generator  MExpr))       ; Is the object at an address a primitive procedure?
(: cg-op-apply-primitive-procedure (Generator2 MExpr MExpr)) ; Calls the primitive procedure with a code pointer and argument list.
(: cg-op-lookup-variable-value     (Generator2 MExpr MExpr)) ; Returns the object with the given name in the given environment.
(: cg-op-define-variable!          (Generator3 MExpr MExpr MExpr)) ; Creates an object with a given name and value in the given environment
(: cg-op-false?                    (Generator  MExpr))       ; Evaluates to 1 if the expression is 0
(: cg-op-list                      (Generator  MExpr))       ; Creates a 1-element vector with the given object.


(define (cg-op-make-compiled-procedure code env)
  (cg-make-compiled-procedure code env))

(define (cg-op-compiled-procedure-entry obj)
  (append (cg-mexpr obj)
          (cg-read-address-offset (const 1))))

(define (cg-op-compiled-procedure-env obj)
  (append (cg-mexpr obj)
          (cg-read-address-offset (const 2))))
  
(define (cg-op-primitive-procedure? obj)
  (append (cg-mexpr (const TAG-PRIMITIVE-PROCEDURE))
          (cg-mexpr obj)
          (cg-eq-binop)))

(define (cg-op-apply-primitive-procedure proc argl)
  (append (cg-list->stack argl)
          (cg-mexpr proc)
          (asm 'JUMP))) ; The return label must be on the stack

; Could be implemented in Pyramid, but lookup-variable-value would be needed to lookup that definition.
; PSEUDOCODE:
;; (define (lookup-variable-value var env)
;;   (define (env-loop env)
;;     (define (scan vars vals)
;;       (if (null? vals)
;;           (env-loop (cdr env))
;;           (if (eq? var (car vars))
;;               (car vals)
;;               (scan (cdr vars) (cdr vals)))))
;;     (let ((frame (car env)))
;;       (scan (car frame)
;;             (cdr frame))))
;;   (env-loop env))
(define (cg-op-lookup-variable-value name env)
  (let ((env-loop    (make-label 'env-loop))
        (scan        (make-label 'scan))
        (scan-else-1 (make-label 'scan))
        (scan-else-2 (make-label 'scan)))
    (let ((lookup-variable-value (make-label 'lookup-variable-value)))
    ; Stack: [ var, env ]
    (append
     (label (const PRIM-LOOKUP-VARIABLE-VALUE))
     ; Stack: [ var, env ]                          ; len = 2
     (label env-loop)
     (asm 'DUP2)    ; [ +env ]                      ; len = 3
     (cg-car stack) ; [ -env, +frame ]              ; len = 3
     (asm 'DUP1)    ; [ +frame ]                    ; len = 4
     (cg-car stack) ; [ -frame ; +fvars ]           ; len = 4
     (asm 'SWAP1)   ; [ fvars <-> frame ]           ; len = 4
     (cg-cdr stack) ; [ -frame ; +fvals ]           ; len = 4
     ; Stack: [ fvals, fvars, var, env ]            ; len = 4
     (label scan)
     (asm 'DUP1)      ; [ +fvals ]                  ; len = 5
     (cg-null? stack) ; [ -fvals; +null? ]          ; len = 5
     (asm 'ISZERO)    ; [ -null?; +non-null? ]      ; len = 5
     (cg-branch scan-else-1 stack) ; [ -non-null? ] ; len = 4
     (cg-pop 2)       ; [ -fvals; fvars ]           ; len = 2
     (asm 'SWAP1)     ; [ env <-> var ]             ; len = 2
     (cg-cdr stack)   ; [ -env; +(cdr env) ]        ; len = 2
     (asm 'SWAP1)     ; [ var <-> env ]             ; len = 2
     (cg-goto (const env-loop)) ;[ var, (cdr env) ] ; len = 2
     ; Stack: [ fvals, fvars, var, env ]            ; len = 4
     (label scan-else-1)
     (asm 'DUP2)       ; [ +fvars ]                 ; len = 5
     (cg-car stack)    ; [ -fvars; +var' ]          ; len = 5
     (asm 'DUP4)       ; [ +var ]                   ; len = 5
     (cg-eq? stack stack) ; [ -var; -var'; +(eq? var var') ] ; len = 4
     (asm 'ISZERO)     ; [ ! (eq? var var'), fvals, fvars, var, env ] ; len = 5
     (cg-branch scan-else-2 stack) ; [ -! (eq? var var') ] ; len = 4
     (cg-car stack)    ; [ -fvals; +val ]           ; len = 4
     (asm 'SWAP4)      ; [ fvars, var, env, val ]   ; len = 4
     (cg-pop 3)        ; [ -fvars; -var; -env ]     ; len = 1
     (cg-goto (reg 'continue)) ; [ val ]            ; len = 1
     ; Stack: [ fvals, fvars, var, env ]            ; len = 4
     (label scan-else-2)
     (cg-cdr stack) ; [ -fvals; +(cdr fvals)  ]     ; len = 4
     (asm 'SWAP1)   ; [ (cdr fvals) <-> fvars ]     ; len = 4
     (cg-cdr stack) ; [ -fvars; +(cdr fvars)  ]     ; len = 4
     (asm 'SWAP1)   ; [ (cdr fvals) <-> (cdr fvars) ] ; len = 4
     (cg-goto (const scan))))) ; [ fvals, fvars, var, env ]

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
(define (cg-op-define-variable! name value env)
  (let ((scan        (make-label 'scan))
        (scan-else-1 (make-label 'scan))
        (scan-else-2 (make-label 'scan)))
    (append
     (cg-mexpr env)    ; [ +env ]
     (cg-mexpr value)  ; [ +value ]
     (cg-mexpr name)   ; [ +name ]
     ; Stack:            [ name; value; env ]              ; len = 3
     (asm 'DUP3)       ; [ +env; ]                         ; len = 4
     (cg-car stack)    ; [ -env; +frame ]                  ; len = 4
     ; Stack:            [ frame; name; value; env ]       ; len = 4
     (asm 'DUP1)       ; [ +frame ]                        ; len = 5
     (cg-cdr stack)    ; [ -frame; +fvals ]                ; len = 5
     (asm 'SWAP1)      ; [ frame <=> fvals ]               ; len = 5
     (cg-car stack)    ; [ -frame; +fvars ]                ; len = 5
     ; Stack:            [ fvars; fvals; name; value; env ]; len = 5
     (label scan)
     (asm 'DUP1)       ; [ +fvars ]                        ; len = 6
     (cg-null? stack)  ; [ -fvars; +null? ]                ; len = 6
     (asm 'ISZERO)     ; [ -null? ; + !null? ]             ; len = 6
     (cg-branch scan-else-1 stack) ; [ - ! null? ]         ; len = 5
     (cg-pop 2)        ; [ -fvars; -fvals ]                ; len = 3
     (cg-add-binding-to-frame stack stack stack) ; [ -name; -value; -env ] ; len = 0
     (cg-goto (reg 'continue)) ; []                        ; len = 0
     ; Stack:            [ fvars; fvals; name; value; env ]; len = 5
     (label scan-else-1)
     (asm 'DUP3)       ; [ +name ]                         ; len = 6
     (asm 'DUP1)       ; [ +fvars ]                        ; len = 7
     (cg-car stack)    ; [ -fvars; name' ]                 ; len = 7
     (cg-eq? stack stack) ; [ -name'; -name; +(eq? name' name)]; len = 6
     (asm 'ISZERO)     ; [ -(eq? name name'); name != name']; len = 6
     (cg-branch scan-else-2 stack) ; [ - name != name' ]   ; len = 5
     ; Stack:            [ fvars; fvals; name; value; env ]; len = 5
     (asm 'SWAP1)      ; [ fvals <-> fvars ]               ; len = 4
     (asm 'DUP4)       ; [ +value ]                        ; len = 5
     (cg-reverse 2)    ; [ fvals <-> value ]               ; len = 5
     (cg-set-car! stack stack) ; [ -value; -fvals ]        ; len = 3
     (cg-pop 3)        ; [ -name; -value; -env ]           ; len = 0
     (cg-goto (reg 'continue))                             ; len = 0
     ; Stack             [ fvars; fvals; name; value; env ]; len = 5
     (label scan-else-2)
     (cg-cdr stack)    ; [ -fvars; +(cdr fvars) ]          ; len = 5
     (cg-reverse 2)    ; [ fvals <-> fvars ]               ; len = 5
     (cg-cdr stack)    ; [ -fvals; +(cdr fvals) ]          ; len = 5
     (cg-reverse 2)    ; [ fvals <-> fvars ]               ; len = 5
     (cg-jump scan))))

;;; Lists

(: cg-null?        (Generator  MExpr))
(: cg-pair?        (Generator  MExpr))
(: cg-car          (Generator  MExpr))
(: cg-cdr          (Generator  MExpr))
(: cg-list->stack  (Generator  MExpr))
(: cg-list->vector (Generator  MExpr))
(: cg-cons         (Generator2 MExpr Mexpr))
(: cg-set-car!     (Generator2 MExpr MExpr))
(: cg-set-cdr!     (Generator2 MExpr Mexpr))
(: cg-list-length  (Generator  MExpr))


(define (cg-null? exp)
  (append (cg-mexpr exp)
          (cg-tag stack)
          (cg-eq? stack (const TAG-NIL))))

(define (cg-car exp) 
  (cg-read-address-offset exp (const 1)))

(define (cg-cdr exp)
  (cg-read-address-offset exp (const 2)))

(define (cg-list->stack exp)
  (append (cg-list->vector exp)
          (cg-vector->stack stack)))     

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
(define (cg-list->vector exp)
  (let ((loop (make-label 'loop))
        (term (make-label 'term)))
    (append
     (cg-mexpr exp)                      ; [ +list ]
     ; 1. Calculate the length of the list
     (asm 'DUP1)                         ; [ +list ]
     (cg-list-length stack)              ; [ -list; +len ]
     ; 2. Allocate a vector of that size
     (cg-make-vector stack '())          ; [ -len; +vector ]
     ; 3. Loop through the list, setting vector elements.
     ; STACK:                            [ i; vector; list ]
     (label loop)
     ; 4. Check if loop should be terminated
     (asm 'SWAP2)                        ; [ list; vector; i ]
     (asm 'DUP1)                         ; [ list; list; vector; i]
     (cg-null? stack)                    ; [ term?; list; vector; i ]
     (cg-branch term stack)              ; [ list; vector; i ]
     (asm 'SWAP2)                        ; [ i; vector; list ]
     ; 5. Set vector element, and repeat loop.
     ; STACK:                              [ i; vector; list ]
     (asm 'DUP1)                         ; [ i; i; vector; list ]
     (asm 'DUP4)                         ; [ list; i; i; vector; list ]
     (cg-car stack)                      ; [ x; i; i; vector; list ]
     (asm 'SWAP1)                        ; [ i; x; i; vector; list ]
     (asm 'DUP4)                         ; [ vector; i; x; i; vector; list ]
     (cg-vector-write stack stack stack) ; [ -vector; -i; -x ]
     (cg-add stack (const 1))            ; [ i'; vector; list ]
     (asm 'SWAP2)                        ; [ list; vector; i' ]
     (cg-cdr stack)                      ; [ list'; vector; i' ]
     (asm 'SWAP2)                        ; [ i'; vector; list' ]
     (cg-goto loop)
     ; STACK:                              [ list; vector; i ]
     (label term)
     (cg-pop 3)                          ; [ ]
     (cg-goto 'continue)
     
    
(define cg-cons cg-make-pair)

(define (cg-set-car! exp)
  (cg-write-address-offset exp (const 1)))

(define (cg-set-cdr! exp)
  (cg-write-address-offset exp (const 2)))

; PSEUDOCODE
#|
(define (list-length list)
  (define (loop len)
    (if (pair? list)
        (loop (+ 1 len))
        len))
  (loop 0))
|#
(define (cg-list-length exp)
  (let ((loop      (make-label 'loop))
        (terminate (make-label 'terminate)))
    (append
     (eth-push 1 0)           ; [ +len ]
     (cg-mexpr exp)           ; [ +list ]
     ; STACK                    [ list ; len ]
     (label loop)
     (asm 'DUP1)              ; [ +list ]
     (cg-pair? exp)           ; [ -list; + pair? ]
     (cg-op-false? stack)     ; [ -pair?; + ! pair? ]
     (cg-branch terminate)    ; [ - ! pair? ]
     (asm 'SWAP1)             ; [ len <-> list ]
     (cg-add stack (const 1)) ; [ -len; +len' ]
     (asm 'SWAP1)             ; [ list <-> len' ]
     (cg-cdr stack)           ; [ -list; +list' ]
     (cg-goto (const 'loop))  ; [ list'; len' ]
     ; STACK                    [ list; len ]
     (label terminate)
     (cg-pop 1)               ; [ len ]
     (cg-goto (reg 'continue)))))

;;; Vectors

(: cg-vector->stack (Generator MExpr))
(: cg-vector-len (Generator MExpr))
(: cg-vector-write (Generator3 MExpr MExpr MExpr)) ; vector -> offset -> value
(: cg-vector-read (Generator2 MExpr MExpr)) ; vector -> offset

; PSEUDOCODE

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

(define (cg-vector->stack vec)
  (let ((loop (make-label 'loop))
        (term (make-label 'term)))
    (append
     (cg-mexpr vec)               ; [ vec ]
     (asm 'DUP1)                  ; [ vec; vec ]
     (cg-vector-len stack)        ; [ i; vec ]
     (label loop)
     (asm 'DUP1)                  ; [ i; i; vec ]
     (cg-eq? stack (const 0))     ; [ eq?; i; vec ]
     (cg-branch term stack)       ; [ i; vec ]
     (asm 'DUP1)                  ; [ i; i; vec ]
     (cg-sub stack 1)             ; [ i'; i; vec ]
     (asm 'DUP3)                  ; [ vec; i'; i; vec ]
     (cg-vector-read stack stack) ; [ x; i; vec ]
     (asm 'SWAP2)                 ; [ vec; i; x ]
     (asm 'SWAP1)                 ; [ i; vec; x ]
     (cg-goto loop)               ; [ i; vec; x ]
     (label term)
     (cg-pop 2)                   ; [ xs ]
     (cg-goto 'continue))))

(define (cg-vector-len vec)
  (cg-read-address-offset vec (const 2)))

(define (cg-vector-write vec os val)
  (append
   (cg-mexpr val)           ; [ val ]
   (cg-mexpr os)            ; [ os; val ]
   (cg-mexpr vec)           ; [ vec; os; val ]
   (asm 'SWAP1)             ; [ os; vec; val ]
   (cg-add stack (const 3)) ; [ os'; vec; val ]
   (asm 'SWAP1)             ; [ vec; os'; val ]
   (cg-write-address stack stack stack) ; [ ]
   (cg-goto 'continue)))
   
(define (cg-vector-read vec os)
  (append
   (cg-mexpr os)                 ; [ os ]
   (cg-mexpr vec)                ; [ vec; os ]
   (asm 'SWAP1)                  ; [ os; vec ]
   (cg-add stack (const 3))      ; [ os'; vec ]
   (asm 'SWAP1)                  ; [ vec; os' ]
   (cg-read-address stack stack) ; [ x ]
   (cg-goto 'continue)))
   

;;; Arithmetic

; The arity of nullop, unop, binop refer to the number of values popped from the stack.
; So, cg-eq has a net stack impact of 2 pops
   
(: cg-eq? (Generator2 MExpr MExpr))
(: cg-mul (Generator2 MExpr MExpr))
(: cg-add (Generator2 MExpr MExpr))

;;; Runtime support
 
(: cg-make-fixnum              (Generator  MExpr))
(: cg-make-symbol              (Generator  MExpr))
(: cg-make-compiled-procedure  (Generator2 MExpr MExpr))
(: cg-make-primitive-procedure (Generator2 MExpr))
(: cg-make-pair                (Generator2 MExpr MExpr))
(: cg-make-vector              (Generator2 MExpr MExprs))
(: cg-make-nil                 (Generator  Nothing))
(: cg-add-binding-to-frame     (Generator3 MExpr MExpr Mexpr))



; PSEUDOCODE
;; (define (add-binding-to-frame! var val frame)
;;  (set-car! frame (cons var (car frame)))
;;  (set-cdr! frame (cons val (cdr frame))))
(define (cg-add-binding-to-frame name value frame)
  (append (cg-mexpr env)            ; [ +env ]                ; len = 1
          (cg-mexpr value)          ; [ +value ]              ; len = 2
          (cg-mexpr name)           ; [ +name ]               ; len = 3
          ; STACK                     [ name; value; frame ]  ; len = 3
          (asm 'DUP3)               ; [ +frame ]              ; len = 4
          (cg-car stack)            ; [ -frame; +vars ]       ; len = 4
          (asm 'SWAP1)              ; [ vars <-> name ]       ; len = 4
          (cg-cons stack stack )    ; [ -vars; -name; vars' ] ; len = 3
          (asm 'DUP4)               ; [ +frame ]              ; len = 4
          (cg-set-car! stack stack) ; [ -frame; -vars' ]      ; len = 2
          ; STACK                     [ 
          (asm 'DUP2)               ; [ +frame ]              ; len = 3
          (cg-cdr stack)            ; [ -frame; +vals ]       ; len = 3
          (asm 'SWAP1)              ; [ vals <-> value ]      ; len = 3
          (cg-cons stack stack )    ; [ -vals; -value; vals'] ; len = 2
          (asm 'SWAP1)              ; [ frame <-> vals' ]     ; len = 2
          (cg-set-car! stack stack) ; [ -frame; -vals' ]      ; len = 0
          ))

(define (cg-make-compiled-procedure code env)
  (cg-allocate-initialize (const 2) (list code env)))
   
; Returns a pointer to a newly-allocated block of 256-bit words.
(: cg-allocate (Generator MExpr))
(define (cg-allocate size)
  (append
   (cg-mexpr exp)                                 ; [ size ]
   (cg-read-address (const MEM-ALLOCATOR))        ; [ ptr; size ]
   (asm 'DUP1)                                    ; [ ptr; ptr; size ]
   (asm 'SWAP2)                                   ; [ size; ptr; ptr ]
   (cg-add stack stack)                           ; [ ptr'; ptr ]
   (cg-write-address (const MEM-ALLOCATOR) stack) ; [ ptr ]
   (cg-goto 'continue)))

; Allocates a block of memory and initializes each word from the argument list.
(: cg-allocate-initialize (Generator2 MExpr MExprs))

(: cg-read-address        (Generator MExpr))
(: cg-read-address-offset (Generator2 MExpr MExpr))

(: cg-read-address (Generator MExpr))
(define (cg-read-address addr)
  (append (cg-mexpr addr)
          (asm 'MLOAD)))

(: cg-write-address (Generator2 MExpr MExpr))
(define (cg-write-address dest val)
  (append
   (cg-mexpr val)
   (cg-mexpr dest)
   (asm 'MSTORE)))

; Record that a label is located at a specific Ethereum bytecode offset.
(: mark-label (-> LabelName Address Void))
(define (mark-label name addr)
  (push-mlist! label-map (cons name addr)))

; Instructions

(: cg-goto    (Generator  MExpr))
(: cg-branch  (Generator2 MExpr MExpr))
(: cg-reverse (Generator  Fixnum))
(: cg-pop     (Generator  Fixnum))
(: cg-shuffle (Generator (Listof Fixnum)))
#|
SHUFFLE ALGORITHM
We want to emit a sequence of top-level stack swaps that changes [1,2,3,4] to [3,4,1,2]
1. Locate the element that should be relocated to the deepest spot.
2. Swap it to the front, or do nothing if it's already there.
3. Swap it to the back.
4. Recurse onto the (n-1) length stack, possibly with a different desired ordering.

Example:
[1,2,3,4] -> 2 is the deepest element of [3,4,1,2]
[2,1,3,4] -> swap 2 to the front
[4,1,3,2] -> swap 2 to the back
Rename [4,1,3] to [1,2,3] in the desired solution, and repeat
|#
(define stack (reg 'val))
