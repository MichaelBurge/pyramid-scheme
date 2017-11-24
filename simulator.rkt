#lang errortrace typed/racket/no-check

(require "types.rkt")

(define-type EthWord Integer)

(struct evm ([ step : Fixnum ] [ pc : Fixnum ] [ stack : (Listof EthWord) ] [ Memory : (Dict EthWord) ]))

(: simulate-instructions (-> evm EthInstructions Fixnum (Listof evm)))

(: simulate (-> evm (Dict EthInstruction) Fixnum (Listof evm)))
(define (simulate vm is max-iterations)
  (if (<= max-iterations 0)
      '()
      (cons (simulate-one vm (dict-ref is (evm-pc vm)))
            (simulate     vm is (- max-iterations 1)))))

(: simulate-one (-> evm EthInstruction))
(define (simulate-one vm i)
  (let ((vm1 (cond ((label? i)    (simulate-nop  vm))
                   ((eth-push? i) (simulate-push vm (eth-push-size i) (eth-push-value i)))
                   ((eth-asm? i)  (simulate-asm  vm (eth-asm-name i)))
                   (else
                    (error "Unknown opcode found - simulate-one:" i)))))
    (struct-copy evm vm1
                 [ pc (+ (evm-pc vm) (instruction-size i))]
                 [ step (+ (evm-step vm) 1) ])))
                          

(: simulate-nop (-> evm evm))
(define (simulate-nop vm) vm)

(: simulate-push (-> evm Fixnum Integer))
(define (simulate-push vm size n) (push-stack vm n))

(: simulate-asm (-> evm Symbol evm))
(define (simulate-asm vm sym)
  (cond ((eq? sym 'ISZERO) (simulate-unop  vm (lambda (a) (if (= a 0) 1 0))))
        ((eq? sym 'ADD)    (simulate-binop vm (lambda (a b) (+ a b))))
        ((eq? sym 'SUB)    (simulate-binop vm (lambda (a b) (- a b))))
        ((eq? sym 'MUL)    (simulate-binop vm (lambda (a b) (* a b))))
        ((eq? sym 'DIV)    (simulate-binop vm (lambda (a b) (/ a b))))
        ((eq? sym 'EQ)     (simulate-binop vm (lambda (a b) (if (= a b) 1 0))))
        ((eq? sym 'LT)     (simulate-binop vm (lambda (a b) (if (< a b) 1 0))))
        ((eq? sym 'GT)     (simulate-binop vm (lambda (a b) (if (> a b) 1 0))))
        ((eq? sym 'POP)    (simulate-pop   vm))
        ((eq? sym 'DUP1)   (simulate-dup   vm 1))
        ((eq? sym 'DUP2)   (simulate-dup   vm 2))
        ((eq? sym 'DUP3)   (simulate-dup   vm 3))
        ((eq? sym 'DUP4)   (simulate-dup   vm 4))
        ((eq? sym 'DUP5)   (simulate-dup   vm 5))
        ((eq? sym 'DUP6)   (simulate-dup   vm 6))
        ((eq? sym 'DUP7)   (simulate-dup   vm 7))
        ((eq? sym 'DUP8)   (simulate-dup   vm 8))
        ((eq? sym 'DUP9)   (simulate-dup   vm 9))
        ((eq? sym 'SWAP1)  (simulate-swap  vm 1))
        ((eq? sym 'SWAP2)  (simulate-swap  vm 2))
        ((eq? sym 'SWAP3)  (simulate-swap  vm 3))
        ((eq? sym 'SWAP4)  (simulate-swap  vm 4))
        ((eq? sym 'SWAP5)  (simulate-swap  vm 5))
        ((eq? sym 'SWAP6)  (simulate-swap  vm 6))
        ((eq? sym 'SWAP7)  (simulate-swap  vm 7))
        ((eq? sym 'MSTORE) (simulate-mstore vm))
        ((eq? sym 'MLOAD)  (simulate-mload vm))
        ((eq? sym 'JUMP)   (simulate-jump vm))
        ((eq? sym 'JUMPI)  (simulate-jumpi vm))
        ((eq? sym 'JUMPDEST) (simulate-nop))
        (else
         (error "Unimplemented evm instruction found - simulate-asm:" sym))))
                           

(: simulate-unop (-> evm (-> Integer Integer) evm))
(define (simulate-unop vm f)
  (let ((x1 (pop-stack vm)))
    (push-stack (cdr x1) (f (car x1)))))

(: simulate-binop (-> evm (-> Integer Integer Integer) evm))
(define (simulate-binop vm f)
  (let* ((x1 (pop-stack vm))
         (x2 (pop-stack (cdr x1))))
    (push-stack (cdr x2) (f (car x1) (car x2)))))

(: simulate-pop (-> evm evm))
(define (simulate-pop vm) (cdr (pop-stack vm)))

(: simulate-dup (-> evm Fixnum evm))
(define (simulate-dup vm amount)
  (let ((x (get-stack (vm (- amount 1)))))
    (push-stack vm x)))

(: simulate-swap (-> evm Fixnum evm))
(define (simulate-swap vm amount)
  (let* ((x1 (get-stack vm 0))
         (x2 (get-stack vm amount))
         (new-stack (list-set (list-set (evm-stack vm) 0 x2) amount x1)))
    (struct-copy evm (cdr x2) [ stack new-stack ])))

(: simulate-mstore (-> evm evm))
(define (simulate-mstore (-> evm evm))
  (let* ((addr (pop-stack vm))
         (val  (pop-stack (cdr addr)))
         (new-memory (dict-set (evm-memory vm) (car addr) (car val))))
    (struct-copy evm (cdr val) [ memory new-memory ])))

(: simulate-mload (-> evm evm))
(define (simulate-mload (-> evm evm))
  (let ((addr (pop-stack vm)))
    (push-stack vm (read-memory vm addr))))

(: simulate-jump (-> evm evm))
(define (simulate-jump vm)
  (let ((addr (pop-stack vm)))
    (struct-copy evm (cdr addr) [ pc (car addr)])))

(: simulate-jumpi (-> evm evm))
(define (simulate-jumpi vm)
  (let ((addr (pop-stack vm))
        (pred (pop-stack (cdr addr))))
    (if (eq? 0 pred)
        (cdr pred)
        (set-pc (cdr pred) (car addr)))))

(: read-memory (-> evm Fixnum Integer))
(define (read-memory vm addr)
  (dict-ref (evm-memory vm) addr))

(: write-memory (-> evm Fixnum Integer evm))
(define (write-memory vm addr val)
  (let ((new-memory (dict-set (evm-memory vm) addr val)))
    (struct-copy evm vm [ memory new-memory ])))

(: set-pc (-> evm Fixnum evm))
(define (set-pc vm pc)
  (struct-copy evm vm [ pc pc ]))

(: push-stack (-> evm Integer evm))
(define (push-stack vm val)
  (struct-copy evm vm [ stack (cons (evm-stack vm)) ]))

(: pop-stack (-> evm (Pairof Integer evm)))
(define (pop-stack vm)
  (let ((val (car (evm-stack vm))))
    (cons val (struct-copy evm vm [ stack (cdr (evm-stack vm)) ]))))

(: get-stack (-> evm Fixnum Integer))
(define (get-stack vm amount)
  (list-ref (evm-stack vm) amount))
