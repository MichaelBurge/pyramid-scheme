#lang typed/racket

(require "types.rkt")
(require "ast.rkt")
(require "globals.rkt")
(require "utils.rkt")
(require racket/set)
(provide (all-defined-out))

; Global constants
(define reg-val : RegisterName 'val)
(define linkage-next : Target 'next)
(define linkage-return : Target 'return)

; Global variables
(define label-counter 0)

(: compile-pyramid (-> Target Linkage Pyramid inst-seq))
(define (compile-pyramid target linkage exp)
  (match exp
    [(struct pyr-const (x))                (compile-const target linkage x)]
    [(struct pyr-quoted (x))               (compile-quoted target linkage x)]
    [(struct pyr-asm _)                    (inst-seq '() '() (list exp))]
    [(struct pyr-macro-definition _)       (compile-macro-definition target linkage exp)]
    [(struct pyr-variable (name))          (compile-variable target linkage name)]
    [(struct pyr-assign (name val))        (compile-assignment target linkage name val )]
    [(struct pyr-definition (name val))    (compile-definition target linkage name val )]
    [(struct pyr-if (pred cons alt))       (compile-if target linkage pred cons alt)]
    [(struct pyr-lambda (parameters body)) (compile-lambda target linkage parameters body)]
    [(struct pyr-begin (actions))          (compile-sequence target linkage actions)]
    [(struct pyr-macro-application _)      (compile-macro-application target linkage exp)]
    [(struct pyr-application (op args))    (compile-application target linkage op args)]
    [_ (error "compile-pyramid: Unknown expression" exp)]))

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

(: compile-const (-> Target Linkage RegisterValue inst-seq))
(define (compile-const target linkage val)
  (end-with-linkage linkage
                    (inst-seq '() (list target)
                              (list (assign target (boxed-const val))))))

(: compile-quoted (-> Target Linkage Pyramid inst-seq))
(define (compile-quoted target linkage exp)
  (match exp
    [(struct pyr-const (val)) (compile-const target linkage val)]
    [(struct pyr-variable (name))
     (end-with-linkage linkage
                       (inst-seq '() (list target)
                                 (list (assign target (boxed-const name)))))]
    [(struct pyr-application (hd tl))
     (end-with-linkage linkage
                       (append-instruction-sequences (construct-arglist (map (λ ([ x : Pyramid ])
                                                                               (compile-quoted 'val 'next x))
                                                                             (cons hd tl)))
                                                     (inst-seq '(argl) (listof target)
                                                               (list (assign target (reg 'argl))))))]
    [else (error "compile-quoted: Unhandled case" exp)]))
  

(: compile-macro-definition (-> Target Linkage pyr-macro-definition inst-seq))
(define (compile-macro-definition target linkage exp)
  (install-macro-exp! exp)
  (inst-seq '() '() '())
  )

(: compile-variable (-> Target Linkage VariableName inst-seq))
(define (compile-variable target linkage name)
  (end-with-linkage linkage
                    (inst-seq '(env) (list target)
                              (list (assign target
                                            (op 'lookup-variable-value
                                                `(,(const name)
                                                  ,(reg 'env))))))))

(: compile-assignment (-> Target Linkage RegisterName Pyramid inst-seq))
(define (compile-assignment target linkage name val)
  (let ([value-code (compile-pyramid 'val 'next val)])
    (end-with-linkage linkage
                      (preserving '(env)
                                  value-code
                                  (inst-seq '(env val) (list target)
                                            (listof (perform (op 'set-variable-value!
                                                                 (listof (const name)
                                                                         (reg 'val)
                                                                         (reg 'env))))))))))

(: compile-definition (-> Target Linkage VariableName Pyramid inst-seq))
(define (compile-definition target linkage name val)
  (let ([value-code (compile-pyramid 'val 'next val)])
    (end-with-linkage linkage
                      (preserving '(env)
                                  value-code
                                  (inst-seq '(env val) (list target)
                                            (listof (perform (op 'define-variable!
                                                                 `(,(const name)
                                                                   ,(reg 'val)
                                                                   ,(reg 'env))))))))))


(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(: make-label (case-> (-> Symbol label-definition)
                      (-> Symbol Integer label-definition)))
   
(define (make-label name [offset 0])
  (label-definition (string->symbol
                     (string-append (symbol->string name)
                                    "-"
                                    (number->string (*abstract-offset*))
                                    "-"
                                    (number->string (new-label-number))))
                    offset
                    #f
                    ))

(: compile-if (-> Target Linkage Pyramid Pyramid Pyramid inst-seq))
(define (compile-if target linkage pred con alt)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile-pyramid 'val 'next pred))
            (c-code (compile-pyramid target consequent-linkage con))
            (a-code (compile-pyramid target linkage alt)))
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

(: compile-sequence (-> Target Linkage Pyramids inst-seq))
(define (compile-sequence target linkage seq)
  (match seq
    ('()              (empty-instruction-sequence))
    (`(,x)       (compile-pyramid target linkage x))
    (`(,x . ,xs) (preserving '(env continue)
                                  (compile-pyramid  target 'next   x)
                                  (compile-sequence target linkage xs)))))

(: compile-lambda (-> Target Linkage VariableNames Pyramid inst-seq))
(define (compile-lambda target linkage formals body)
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
        (compile-lambda-body proc-entry formals body))
       after-lambda))))

(: compile-lambda-body (-> label-definition VariableNames Pyramid inst-seq))
(define (compile-lambda-body proc-entry formals body)
  (append-instruction-sequences
   (inst-seq '(env proc argl) '(env)
             (listof proc-entry
                     (assign 'env (op 'compiled-procedure-env `(,(reg 'proc))))
                     (assign 'env (op 'extend-environment
                                      `(,(const formals)
                                        ,(reg 'argl)
                                        ,(reg 'env))))))
   (compile-pyramid 'val 'return body)))


(: compile-application (-> Target Linkage Pyramid Pyramids inst-seq))
(define (compile-application target linkage op args)
  (let ([proc-code (compile-pyramid 'proc 'next op)]
        [operand-codes
         (map (λ ([ arg : Pyramid ])
                (compile-pyramid 'val 'next arg))
              args)])
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))  

(: compile-macro-application (-> Target Linkage pyr-macro-application inst-seq))
(define (compile-macro-application target linkage exp)
    (compile-pyramid target linkage (expand-macro exp)))

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
                                     (listof target)
                                     (listof (assign target
                                                     (op 'apply-primitive-procedure
                                                         `(,(reg 'proc)
                                                           ,(reg 'argl)))))))))
       after-call))))

(: compile-proc-appl (-> Target (U 'return label) inst-seq))
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (inst-seq '(proc) all-regs
                   (listof (assign 'continue linkage)
                           (assign 'val (op 'compiled-procedure-entry
                                            `(,(reg 'proc))))
                         (goto (reg 'val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (inst-seq '(proc) all-regs
                     (listof (assign 'continue proc-return)
                             (assign 'val (op 'compiled-procedure-entry
                                              `(,(reg 'proc))))
                             (goto (reg 'val))
                             proc-return
                             (assign target (reg 'val))
                             (goto linkage)))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (inst-seq '(proc continue) all-regs
                   (listof (assign 'val (op 'compiled-procedure-entry
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
  (if (label? s)
      (list s)
      (inst-seq-statements s)))

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
