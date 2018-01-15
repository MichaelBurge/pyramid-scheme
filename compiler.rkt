#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "ast.rkt")
(require "interpreter.rkt")
(require "globals.rkt")
(require racket/set)
(provide (all-defined-out))

; Global constants
(define reg-val : RegisterName 'val)
(define linkage-next : Target 'next)
(define linkage-return : Target 'return)

; Global variables
(define label-counter 0)

(: make-pyramid-machine (-> ControllerText Machine))
(define (make-pyramid-machine text)
  (make-machine all-regs '() text))

(: compile-pyramid (-> Pyramid Target Linkage inst-seq))
(define (compile-pyramid exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((asm? exp) (inst-seq '() '() (list exp)))
        ((macro? exp) (compile-macro exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((macro-application? exp) (compile-macro-application exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(: empty-instruction-sequence (-> inst-seq))
(define (empty-instruction-sequence)
  (inst-seq '() '() '()))

(: compile-linkage (-> Linkage inst-seq))
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (inst-seq '(continue) '()
                   (list (goto (reg 'continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (inst-seq '() '()
                   (list (goto linkage))))))

(: end-with-linkage (-> Linkage inst-seq inst-seq))
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(: compile-self-evaluating (-> PyrSelfEvaluating Target Linkage inst-seq))
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (inst-seq '() (list target)
                              (list (assign target (boxed-const exp))))))

(: compile-quoted (-> PyrQuote Target Linkage inst-seq))
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (inst-seq '() (list target)
                              (list (assign target (boxed-const (text-of-quotation exp)))))))

(: install-macro! (-> Symbol Procedure Void))
(define (install-macro! name func)
  (namespace-set-variable-value! name func #t (*available-macros*))
  )

(: compile-macro (-> Pyramid Target Linkage inst-seq))
(define (compile-macro exp target linkage)
  (let* ((mac-name (macro-variable exp))
         (arg-names (macro-args exp))
         (mac-body (macro-body exp))
         (macro-exp `(lambda ,arg-names ,mac-body))
         (macro (eval macro-exp (*macro-namespace*)))
         )
    (install-macro! mac-name macro)
    (inst-seq '() '() '())
    ))


(: compile-variable (-> PyrVariable Target Linkage inst-seq))
(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (inst-seq '(env) (list target)
                              (list (assign target
                                            (op 'lookup-variable-value
                                                `(,(const exp)
                                                  ,(reg 'env))))))))

(: compile-assignment (-> PyrAssign Target Linkage inst-seq))
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile-pyramid (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (inst-seq '(env val) (list target)
                                            (list (perform (op 'set-variable-value!
                                                               `(,(const var)
                                                                 ,(reg 'val)
                                                                 ,(reg 'env))))))))))

(: compile-definition (-> PyrDefinition Target Linkage inst-seq))
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile-pyramid (definition-value exp) reg-val linkage-next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (inst-seq '(env val) (list target)
                                            (list (perform (op 'define-variable!
                                                               `(,(const var)
                                                                 ,(reg 'val)
                                                                 ,(reg 'env))))))))))


(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(: make-label (-> Symbol LabelName))
(define (make-label name)
  (label (string->symbol
          (string-append (symbol->string name)
                         "-"
                         (number->string (*abstract-offset*))
                         "-"
                         (number->string (new-label-number))))))

(: compile-if (-> PyrIf Target Linkage inst-seq))
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile-pyramid (if-predicate exp) 'val 'next))
            (c-code
             (compile-pyramid
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile-pyramid (if-alternative exp) target linkage)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (inst-seq '(val) '()
                               (list (test (op 'false? `(,(reg 'val))))
                                     (branch f-branch)))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

(: compile-sequence (-> Sequence Target Linkage inst-seq))
(define (compile-sequence seq target linkage)
  (cond ((null? seq)     (inst-seq '() '() '()))
        ((last-exp? seq) (compile-pyramid (first-exp seq) target linkage))
        (else            (preserving '(env continue)
                                     (compile-pyramid (first-exp seq) target 'next)
                                     (compile-sequence (rest-exps seq) target linkage)))))

(: compile-lambda (-> PyrLambda Target Linkage inst-seq))
(define (compile-lambda [exp : PyrLambda] [target : Target] linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (cast (if (eq? linkage 'next) after-lambda linkage) Linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
                          (inst-seq '(env) (list target)
                                    (list (assign target
                                                  (op 'make-compiled-procedure
                                                      `(,proc-entry
                                                        ,(reg 'env)))))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(: compile-lambda-body (-> PyrLambda LabelName inst-seq))
(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (inst-seq '(env proc argl) '(env)
               (list proc-entry
                     (assign 'env (op 'compiled-procedure-env `(,(reg 'proc))))
                     (assign 'env (op 'extend-environment
                                      `(,(const formals)
                                        ,(reg 'argl)
                                        ,(reg 'env))))))
     (compile-sequence (lambda-body exp) 'val 'return))))


(: compile-application (-> PyrApplication Target Linkage inst-seq))
(define (compile-application exp target linkage)
  (let ((proc-code (compile-pyramid (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda ([ operand : Pyramid ]) (compile-pyramid operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))  

(: compile-macro-application (-> PyrApplication Target Linkage inst-seq))
(define (compile-macro-application exp target linkage)
    (compile-pyramid (expand-macro exp) target linkage))

(: construct-arglist (-> (Listof inst-seq) inst-seq))
(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (inst-seq '() '(argl)
                (list (assign 'argl (const '()))))
      (let ((code-to-get-last-arg
             (append-instruction-sequences
              (car operand-codes)
              (inst-seq '(val) '(argl)
                        (list (assign 'argl (op 'list `(,(reg 'val)))))))))
        (if (null? (cdr operand-codes))
            code-to-get-last-arg
            (preserving '(env)
                        code-to-get-last-arg
                        (code-to-get-rest-args
                         (cdr operand-codes)))))))

(: code-to-get-rest-args (-> (Listof inst-seq) inst-seq))
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (inst-seq '(val argl) '(argl)
                               (list (assign 'argl
                                             (op 'cons
                                                 `(,(reg 'val)
                                                   ,(reg 'argl)))))))))
                                             
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(: compile-procedure-call (-> Target Linkage inst-seq))
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage))
          (primitive-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (inst-seq '(proc) '()
                 (list (test (op 'primitive-procedure? `(,(reg 'proc))))
                       (branch primitive-branch)))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
                           (inst-seq '(continue proc argl)
                                     (list target)
                                     (list (assign target
                                                   (op 'apply-primitive-procedure
                                                       `(,(reg 'proc)
                                                         ,(reg 'argl)))))))))
       after-call))))

(: compile-proc-appl (-> Target Linkage inst-seq))
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (inst-seq '(proc) all-regs
                   (list (assign 'continue linkage)
                         (assign 'val (op 'compiled-procedure-entry
                                          `(,(reg 'proc))))
                         (goto (reg 'val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (inst-seq '(proc) all-regs
                     (list (assign 'continue proc-return)
                           (assign 'val (op 'compiled-procedure-entry
                                            `(,(reg 'proc))))
                           (goto (reg 'val))
                           proc-return
                           (assign target (reg 'val))
                           (goto linkage)))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (inst-seq '(proc continue) all-regs
                   (list (assign 'val (op 'compiled-procedure-entry
                                         `(,(reg 'proc))))
                         (goto (reg 'val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

(: literal-instructions (-> Any Instructions))
(define (literal-instructions x) (cast x Instructions))

(: all-regs RegisterNames)
(define all-regs '(env proc val argl continue))

(: registers-needed (-> iseq-or-label RegisterNames))
(define (registers-needed s)
  (if (label? s) '() (inst-seq-needs s)))

(: registers-modified (-> iseq-or-label RegisterNames))
(define (registers-modified s)
  (if (label? s) '() (inst-seq-modifies s)))

(: statements (-> iseq-or-label Instructions))
(define (statements s)
  (if (label? s) (list s) (inst-seq-statements s)))

(: needs-register? (-> iseq-or-label RegisterName Boolean))
(define (needs-register? seq reg)
  (if (memq reg (registers-needed seq))
      #t
      #f))

(: modifies-register? (-> iseq-or-label RegisterName Boolean))
(define (modifies-register? seq reg)
  (if (memq reg (registers-modified seq))
      #t
      #f))

(: append-instruction-sequences (-> iseq-or-label * inst-seq))
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences [seq1 : iseq-or-label ] [seq2 : iseq-or-label])
    (inst-seq
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (: append-seq-list (-> (Listof iseq-or-label) inst-seq))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(: list-union (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(: list-difference (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(: preserving (-> RegisterNames inst-seq inst-seq inst-seq))
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (inst-seq
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `(,(save first-reg))
                                 (statements seq1)
                                 `(,(restore first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(: tack-on-instruction-sequence (-> iseq-or-label iseq-or-label inst-seq))
(define (tack-on-instruction-sequence seq body-seq)
  (inst-seq
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(: parallel-instruction-sequences (-> iseq-or-label iseq-or-label inst-seq))
(define (parallel-instruction-sequences seq1 seq2)
  (inst-seq
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))


(define (display-macros)
  (display `(,"Mapped symbols:" ,(namespace-mapped-symbols (*available-macros*)))))
