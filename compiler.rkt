#lang typed/racket

(require (submod "types.rkt" ast))
(require (submod "types.rkt" abstract-machine))
(require "ast.rkt")
(require "globals.rkt")
(require "utils.rkt")
(require racket/set)

(provide compile-pyramid
         (all-from-out (submod "types.rkt" abstract-machine)))

; Scoped globals
(: *stack-depth* (Parameterof Integer))
(define *stack-depth* (make-parameter 0))

(define-type Linkage (U 'next 'return label))
(define-type Target RegisterName)

(: compile-pyramid (-> Target Linkage Pyramid inst-seq))
(define (compile-pyramid target linkage exp)
  (match exp
    [(struct pyr-const (x))                (compile-const target linkage x)]
    [(struct pyr-quoted (x))               (compile-quoted target linkage x)]
    [(struct pyr-asm _)                    (compile-asm target linkage exp)]
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
  (make-insts '() '()))

(: compile-linkage (-> Linkage inst-seq))
(define (compile-linkage linkage)
  (match linkage
    ['return (make-insts '(continue) '()
                         (goto (reg 'continue)))]
    ['next   (empty-instruction-sequence)]
    [_       (make-insts '() '()
                         (goto linkage))]))

(: end-with-linkage (-> Linkage inst-seq inst-seq))
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(: compile-const (-> Target Linkage RegisterValue inst-seq))
(define (compile-const target linkage val)
  (define mexpr (if (boolean? val) (const val) (boxed-const val)))
  (end-with-linkage linkage
                    (make-insts '() (list target)
                                (assign target mexpr))))

(: compile-quoted (-> Target Linkage Pyramid inst-seq))
(define (compile-quoted target linkage exp)
  (match exp
    [(struct pyr-const (val)) (compile-const target linkage val)]
    [(struct pyr-variable (name))
     (end-with-linkage linkage
                       (make-insts '() (list target)
                                   (assign target (boxed-const name))))]
    [(struct pyr-application (hd tl))
     (end-with-linkage linkage
                       (append-instruction-sequences (construct-arglist (map (λ ([ x : Pyramid ])
                                                                               (compile-quoted 'val 'next x))
                                                                             (cons hd tl)))
                                                     (make-insts '(argl) (listof target)
                                                                 (assign target (reg 'argl)))))]
    [else (error "compile-quoted: Unhandled case" exp)]))

(: compile-asm (-> Target Linkage pyr-asm inst-seq))
(define (compile-asm target linkage exp)
  (end-with-linkage linkage
                    (inst-seq (set) (set) (pyr-asm-insts exp))))

(: compile-macro-definition (-> Target Linkage pyr-macro-definition inst-seq))
(define (compile-macro-definition target linkage exp)
  (install-macro-exp! exp)
  (make-insts '() '())
  )

(: compile-variable (-> Target Linkage VariableName inst-seq))
(define (compile-variable target linkage name)
  (end-with-linkage linkage
                    (make-insts '(env) (list target)
                                (assign target
                                        (op 'lookup-variable-value
                                            `(,(const name)
                                              ,(reg 'env)))))))

(: compile-assignment (-> Target Linkage VariableName Pyramid inst-seq))
(define (compile-assignment target linkage name val)
  (let ([value-code (compile-pyramid 'val 'next val)])
    (end-with-linkage linkage
                      (preserving '(env)
                                  value-code
                                  (make-insts '(env val) (list target)
                                              (perform (op 'set-variable-value!
                                                           (listof (const name)
                                                                   (reg 'val)
                                                                   (reg 'env)))))))))

(: compile-definition (-> Target Linkage VariableName Pyramid inst-seq))
(define (compile-definition target linkage name val)
  (let ([value-code (compile-pyramid 'val 'next val)])
    (end-with-linkage linkage
                      (preserving '(env)
                                  value-code
                                  (make-insts '(env val) (list target)
                                              (perform (op 'define-variable!
                                                           `(,(const name)
                                                             ,(reg 'val)
                                                             ,(reg 'env)))))))))

(: label->insts (-> label-definition inst-seq))
(define (label->insts lbl)
  (make-insts '() '() lbl))

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
                     (make-insts '(val) '()
                                 (test (op 'false? `(,(reg 'val))))
                                 (branch f-branch))
                     (parallel-instruction-sequences
                      (append-instruction-sequences (label->insts t-branch) c-code)
                      (append-instruction-sequences (label->insts f-branch) a-code))
                     (label->insts after-if)))))))

(: compile-sequence (-> Target Linkage Pyramids inst-seq))
(define (compile-sequence target linkage seq)
  (match seq
    ('()         (compile-linkage linkage))
    (`(,x)       (compile-pyramid target linkage x))
    (`(,x . ,xs) (preserving '(env continue)
                                  (compile-pyramid  target 'next   x)
                                  (compile-sequence target linkage xs)))))

(: compile-lambda (-> Target Linkage VariableNames Pyramid inst-seq))
(define (compile-lambda target linkage formals body)
  (let ((proc-entry   (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
                          (make-insts '(env) (list target)
                                      (assign target
                                              (op 'make-compiled-procedure
                                                  `(,proc-entry
                                                    ,(reg 'env))))))
        (compile-lambda-body proc-entry formals body))
       (label->insts after-lambda)))))

(: compile-lambda-body (-> label-definition VariableNames Pyramid inst-seq))
(define (compile-lambda-body proc-entry formals body)
  (append-instruction-sequences
   (make-insts '(env proc argl) '(env)
               proc-entry
               (assign 'env (op 'compiled-procedure-env `(,(reg 'proc))))
               (assign 'env (op 'extend-environment
                                `(,(const (reverse formals))
                                  ,(reg 'argl)
                                  ,(reg 'env)))))
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
      (make-insts '() '(argl)
                  (assign 'argl (const '())))
      (let ((code-to-get-last-arg
             (append-instruction-sequences
              (car operand-codes)
              (make-insts '(val) '(argl)
                          (assign 'argl (op 'singleton `(,(reg 'val))))))))
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
                     (make-insts '(val argl) '(argl)
                                 (assign 'argl
                                         (op 'pair
                                             `(,(reg 'val)
                                               ,(reg 'argl))))))))
                                             
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))

(: compile-procedure-call (-> Target Linkage inst-seq))
(define (compile-procedure-call target linkage)
  (let ((primitive-branch    (make-label 'primitive-branch))
        (compiled-branch     (make-label 'compiled-branch))
        (continuation-branch (make-label 'continuation-branch))
        (after-call          (make-label 'after-call)))
    (let ([compiled-linkage  (if (eq? linkage 'next) after-call linkage)]
          [primitive-linkage (if (eq? linkage 'next) after-call linkage)]
          )
      (append-instruction-sequences
       (make-insts '(proc) '()
                   (test (op 'compiled-procedure? `(,(reg 'proc))))
                   (branch compiled-branch)
                   (test (op 'continuation? `(,(reg 'proc))))
                   (branch continuation-branch)
                   )
       (parallel-instruction-sequences
        (append-instruction-sequences
         (label->insts primitive-branch)
         (end-with-linkage primitive-linkage
                           (make-insts '(continue proc argl) (listof target)
                                       (assign target
                                               (op 'apply-primitive-procedure
                                                   `(,(reg 'proc)
                                                     ,(reg 'argl)))))))
        (append-instruction-sequences
         (label->insts compiled-branch)
         (compile-proc-appl target compiled-linkage))
        (label->insts continuation-branch)
        (make-insts '(val) '() (perform (op 'restore-continuation!
                                            (list (reg 'proc))))))
       (label->insts after-call)
      ))))

  (: compile-proc-appl (-> Target (U 'return label) inst-seq))
(define (compile-proc-appl target linkage)
  (match (cons target linkage)
    [(cons 'val 'return)
     (make-insts '(proc continue) all-regs
                 (assign 'val (op 'compiled-procedure-entry
                                  `(,(reg 'proc))))
                 (goto (reg 'val)))]
    [(cons _    'return) (error "compile-proc-appl: return linkage implies target should be val" target)]
    [(cons 'val _) (make-insts '(proc) all-regs
                               (assign 'continue linkage)
                               (assign 'val (op 'compiled-procedure-entry
                                                `(,(reg 'proc))))
                               (goto (reg 'val)))]
    [(cons _ _ )
     (let ((proc-return (make-label 'proc-return)))
       (make-insts '(proc) all-regs
                   (assign 'continue proc-return)
                   (assign 'val (op 'compiled-procedure-entry
                                    `(,(reg 'proc))))
                   (goto (reg 'val))
                   proc-return
                   (assign target (reg 'val))
                   (goto linkage)))]
    ))

(: all-regs RegisterNames)
(define all-regs (apply set '(env proc val argl continue)))

(: append-instruction-sequences (-> inst-seq * inst-seq))
(define (append-instruction-sequences . seqs)
  (: append-2-sequences (-> inst-seq inst-seq inst-seq))
  (define (append-2-sequences seq1 seq2)
    (inst-seq
     (set-union (inst-seq-needs seq1)
                (set-subtract (inst-seq-needs seq2)
                              (inst-seq-modifies seq1)))
     (set-union (inst-seq-modifies seq1)
                (inst-seq-modifies seq2))
     (append (inst-seq-statements seq1)
             (inst-seq-statements seq2))))
  (: append-seq-list (-> (Listof inst-seq) inst-seq))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define-syntax-rule (preserving regs seq1 seq2)
  (preserving-lazy (apply set regs) seq1 seq2))

(: preserving-lazy (-> (U (Listof RegisterName) RegisterNames) inst-seq inst-seq inst-seq))
(define (preserving-lazy regs1 seq1 seq2)
  (define regs          (if (list? regs1) (apply set regs1) regs1))
  (define seq1-needs    (inst-seq-needs    seq1))
  (define seq2-needs    (inst-seq-needs    seq2))
  (define seq1-modifies (inst-seq-modifies seq1))
  (define seq2-modifies (inst-seq-modifies seq2))
  (: saved-regs RegisterNames)
  (define saved-regs (set-intersect regs seq2-needs seq1-modifies))
  (: wrap-frame (-> Integer Instructions Instructions Instructions Instructions))
  (define (wrap-frame size pre-sts sts post-sts)
    (if (> size 0)
        (append
         (list (save (reg 'stack-size)))
         pre-sts
         (list (assign 'stack-size (op 'add (list (const (+ 1 size)) (reg 'stack-size)))))
         sts
         post-sts
         (list (restore 'stack-size)))
        (append pre-sts sts post-sts)))
  (append-instruction-sequences
   (inst-seq (set-union    seq1-needs    saved-regs)
             (set-subtract seq1-modifies saved-regs)
             (let* ([ rs (set->list saved-regs)])
               (wrap-frame
                (length rs)
                (map save (map reg rs))
                (inst-seq-statements seq1)
                (map restore (reverse rs)))))
   seq2))



(: tack-on-instruction-sequence (-> inst-seq inst-seq inst-seq))
(define (tack-on-instruction-sequence seq body-seq)
  (inst-seq
   (inst-seq-needs seq)
   (inst-seq-modifies seq)
   (append (inst-seq-statements seq) (inst-seq-statements body-seq))))

(: parallel-instruction-sequences (-> inst-seq inst-seq * inst-seq))
(define (parallel-instruction-sequences seq0 . seqs)
  (inst-seq
   (apply set-union (inst-seq-needs      seq0) (map inst-seq-needs      seqs))
   (apply set-union (inst-seq-modifies   seq0) (map inst-seq-modifies   seqs))
   (apply append    (inst-seq-statements seq0) (map inst-seq-statements seqs))
   ))

(: make-insts (-> (U RegisterNames (Listof RegisterName))
                  (U RegisterNames (Listof RegisterName))
                  Instruction *
                  inst-seq))
(define (make-insts needs modifies . is)
  (let ([ lst-needs    (if (list? needs   ) needs    (set->list needs   ))]
        [ lst-modifies (if (list? modifies) modifies (set->list modifies))]
        )
    (inst-seq (apply set lst-needs) (apply set lst-modifies) is)))


