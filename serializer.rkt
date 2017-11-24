#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "utils.rkt")
(require file/sha1)
(require binaryio/integer)
(require errortrace)

(provide (all-defined-out))

(struct relocation ([ pos : Fixnum ] [ symbol : Symbol ]) #:transparent)

(define-type SymbolTable (Dict Fixnum))
(define-type RelocationTable (Set relocation))

#| Concepts

-- Relocations --
The single pass of the serializer may not have read all labels when it encounters one, so it doesn't know the
numerical value to insert. It generates a relocation, but needs to leave a specific number of bytes.
|#

; Constants

(define assumed-label-size 2) ; TODO: Number of bytes to leave behind for label relocations. This makes it difficult to write programs larger than 65536 bytes.

; Global variables
(define *byte-offset* 0)
(define *symbol-table* (make-hash '()))
(define *relocation-table* (set))

(: opcode-table (Listof opcode))
(define opcode-table
  `(,(opcode #x00 'STOP)
    ,(opcode #x01 'ADD)
    ,(opcode #x02 'MUL)
    ,(opcode #x03 'SUB)
    ,(opcode #x04 'DIV)
    ,(opcode #x05 'SDIV)
    ,(opcode #x06 'MOD)
    ,(opcode #x07 'SMOD)
    ,(opcode #x08 'ADDMOD)
    ,(opcode #x09 'MULMOD)
    ,(opcode #x0a 'EXP)
    ,(opcode #x0b 'SIGNEXTEND)
    
    ,(opcode #x10 'LT)
    ,(opcode #x11 'GT)
    ,(opcode #x12 'SLT)
    ,(opcode #x13 'LGT)
    ,(opcode #x14 'EQ)
    ,(opcode #x15 'ISZERO)
    ,(opcode #x16 'AND)
    ,(opcode #x17 'OR)
    ,(opcode #x18 'XOR)
    ,(opcode #x19 'NOT)
    ,(opcode #x1a 'BYTE)
    
    ,(opcode #x20 'SHA3)
    
    ,(opcode #x30 'ADDRESS)
    ,(opcode #x31 'BALANCE)
    ,(opcode #x32 'ORIGIN)
    ,(opcode #x33 'CALLER)
    ,(opcode #x34 'CALLVALUE)
    ,(opcode #x35 'CALLDATALOAD)
    ,(opcode #x36 'CALLDATASIZE)
    ,(opcode #x37 'CALLDATACOPY)
    ,(opcode #x38 'CODESIZE)
    ,(opcode #x39 'CODECOPY)
    ,(opcode #x3a 'GASPRICE)
    ,(opcode #x3b 'EXTCODESIZE)
    ,(opcode #x3c 'EXTCODECOPY)
    
    ,(opcode #x40 'BLOCKHASH)
    ,(opcode #x41 'COINBASE)
    ,(opcode #x42 'TIMESTAMP)
    ,(opcode #x43 'NUMBER)
    ,(opcode #x44 'DIFFICULTY)
    ,(opcode #x45 'GASLIMIT)
    
    ,(opcode #x50 'POP)
    ,(opcode #x51 'MLOAD)
    ,(opcode #x52 'MSTORE)
    ,(opcode #x53 'MSTORE8)
    ,(opcode #x54 'SLOAD)
    ,(opcode #x55 'SSTORE)
    ,(opcode #x56 'JUMP)
    ,(opcode #x57 'JUMPI)
    ,(opcode #x58 'PC)
    ,(opcode #x59 'MSIZE)
    ,(opcode #x5a 'GAS)
    ,(opcode #x5b 'JUMPDEST)
    
    ,(opcode #x60 'PUSH1)
    ,(opcode #x61 'PUSH2)
    ,(opcode #x62 'PUSH3)
    ,(opcode #x63 'PUSH4)
    ,(opcode #x64 'PUSH5)
    ,(opcode #x65 'PUSH6)
    ,(opcode #x66 'PUSH7)
    ,(opcode #x67 'PUSH8)
    ,(opcode #x68 'PUSH9)
    ,(opcode #x69 'PUSH10)
    ,(opcode #x6a 'PUSH11)
    ,(opcode #x6b 'PUSH12)
    ,(opcode #x6c 'PUSH13)
    ,(opcode #x6d 'PUSH14)
    ,(opcode #x6e 'PUSH15)
    ,(opcode #x6f 'PUSH16)
    ,(opcode #x70 'PUSH17)
    ,(opcode #x71 'PUSH18)
    ,(opcode #x72 'PUSH19)
    ,(opcode #x73 'PUSH20)
    ,(opcode #x74 'PUSH21)
    ,(opcode #x75 'PUSH22)
    ,(opcode #x76 'PUSH23)
    ,(opcode #x77 'PUSH24)
    ,(opcode #x78 'PUSH25)
    ,(opcode #x79 'PUSH26)
    ,(opcode #x7a 'PUSH27)
    ,(opcode #x7b 'PUSH28)
    ,(opcode #x7c 'PUSH29)
    ,(opcode #x7d 'PUSH30)
    ,(opcode #x7e 'PUSH31)
    ,(opcode #x7f 'PUSH32)
    
    ,(opcode #x80 'DUP1)
    ,(opcode #x81 'DUP2)
    ,(opcode #x82 'DUP3)
    ,(opcode #x83 'DUP4)
    ,(opcode #x84 'DUP5)
    ,(opcode #x85 'DUP6)
    ,(opcode #x86 'DUP7)
    ,(opcode #x87 'DUP8)
    ,(opcode #x88 'DUP9)
    ,(opcode #x89 'DUP10)
    ,(opcode #x8a 'DUP11)
    ,(opcode #x8b 'DUP12)
    ,(opcode #x8c 'DUP13)
    ,(opcode #x8d 'DUP14)
    ,(opcode #x8e 'DUP15)
    ,(opcode #x8f 'DUP16)
    
    ,(opcode #x90 'SWAP1)
    ,(opcode #x91 'SWAP2)
    ,(opcode #x92 'SWAP3)
    ,(opcode #x93 'SWAP4)
    ,(opcode #x94 'SWAP5)
    ,(opcode #x95 'SWAP6)
    ,(opcode #x96 'SWAP7)
    ,(opcode #x97 'SWAP8)
    ,(opcode #x98 'SWAP9)
    ,(opcode #x99 'SWAP10)
    ,(opcode #x9a 'SWAP11)
    ,(opcode #x9b 'SWAP12)
    ,(opcode #x9c 'SWAP13)
    ,(opcode #x9d 'SWAP14)
    ,(opcode #x9e 'SWAP15)
    ,(opcode #x9f 'SWAP16)
    
    ,(opcode #xa0 'LOG0)
    ,(opcode #xa1 'LOG1)
    ,(opcode #xa2 'LOG2)
    ,(opcode #xa3 'LOG3)
    ,(opcode #xa4 'LOG3)
    
    ,(opcode #xf0 'CREATE)
    ,(opcode #xf1 'CALL)
    ,(opcode #xf2 'CALLCODE)
    ,(opcode #xf3 'RETURN)
    ,(opcode #xf4 'DELEGATECALL)
    
    ,(opcode #xfd 'REVERT)
    ,(opcode #xfe 'INVALID)
    ,(opcode #xff 'SELFDESTRUCT)
    ))

(define opcodes-by-sym
  (make-hash (map (lambda (op)
                    (cons (opcode-name op) op))
                  opcode-table)))

(define opcodes-by-byte
  (make-hash (map (lambda (op)
                    (cons (opcode-byte op) op))
                  opcode-table)))

(: serialize-print (-> EthInstructions bytes))
(define (serialize-print is)
  (let* ((bs (serialize is)))
    (write (bytes->hex-string bs))
    (newline)
    ; (print-symbol-table *symbol-table*)
    (print-relocations *relocation-table*)
    (apply-relocations! bs *relocation-table* *symbol-table*)
    (write (bytes->hex-string (wrap-loader bs)))
    (newline)
    bs))

; Consider using wrap-loader to prepend an initializer program.
(: serialize (-> EthInstructions bytes))
(define (serialize is)
  (if (null? is)
      (bytes)
      (bytes-append (serialize-one (car is))
                    (serialize     (cdr is)))))

(: serialize-one (-> EthInstruction bytes))
(define (serialize-one i)
  (set! *byte-offset* (+ 1 *byte-offset*))
  (cond ((eth-asm?     i) (serialize-asm i))
        ((eth-push?    i) (serialize-push i))
        ((eth-unknown? i) (bytes i))
        ((label?       i) (serialize-label i))
        (else
         (error "Unknown EthInstruction - serialize-one:" i))))

(: serialize-asm (-> eth-asm bytes))
(define (serialize-asm i)
  (serialize-opcode (lookup-opcode (eth-asm-name i))))

(: serialize-opcode (-> opcode bytes))
(define (serialize-opcode op)
  (bytes (opcode-byte op)))

(: serialize-push (-> eth-push bytes))
(define (serialize-push push)
  (let ((op (lookup-push-opcode push))
        (val (push-true-value push))
        (size (push-true-size push)))
    (set! *byte-offset* (+ size *byte-offset*))
    (bytes-append (bytes (opcode-byte op))
                  (integer->bytes val size #f))))

(: serialize-label (-> label bytes))
(define (serialize-label lbl)
  (remember-label lbl)
  (serialize-asm (eth-asm 'JUMPDEST)))

(: lookup-opcode (-> Symbol opcode))
(define (lookup-opcode sym)
  (dict-ref opcodes-by-sym sym))

(: lookup-push-opcode (-> eth-push opcode))
(define (lookup-push-opcode push)
  (dict-ref opcodes-by-byte (+ #x5f (push-true-size push))))

(: remember-label (-> label Void))
(define (remember-label lbl)
  (dict-set! *symbol-table* lbl (- *byte-offset* 1)))

(: push-true-value (-> eth-push integer))
#| push-true-value:
Either a label or integer can be pushed onto the stack.
* An integer is emitted in big-endian form with the given size.
* A label is either 0 if the label is unknown, or the known value.
* Label values become known when the assembler encounters one. A "current position" global counter is the value.
|#
(define (push-true-value push)
  (let ((val (eth-push-value push)))
    (cond ((label? val) (begin
                          (generate-relocation (relocation *byte-offset* val))
                          (dict-ref *symbol-table* (label-name val) 0)))
          ; Symbols are unexpected: Labels are wrapped in a struct; quotes are expanded to integers in the code generator.
          ((symbol? val) (error "Unexpected symbol - push-true-val" val))
          ((integer? val) val)
          (else
           (error "Unknown value" val)))))

(: push-true-size (-> eth-push Fixnum))
(define (push-true-size push)
  (if (eq? (eth-push-size push) 'shrink)
      (if (eq? (push-true-value push) 0)
          2
          (integer-bytes (push-true-value push)))
      (eth-push-size push)))

(define (print-symbol-table symbols)
  (let ((show (lambda (lbl os)
                (display (label-name lbl))
                (write-char #\tab)
                (display (integer->hex os))
                (newline)))
        (symbols-list (sort (dict->list symbols) < #:key (lambda (x) (cdr x)))))
    (display "Symbol Table:") (newline)    
    (for ([ symbol symbols-list ])
      (show (car symbol) (cdr symbol)))
    (newline)))

(define (print-relocations relocs)
  (display "Relocations:") (newline)
  (for/set ([ reloc relocs ])
    (display `(,(integer->hex (relocation-pos reloc))
               ,(label-name (relocation-symbol reloc))))
    (newline)))
    

(: apply-relocations! (-> bytes RelocationTable LabelMap bytes))
(define (apply-relocations! bs relocs symbols)
  (define (apply-relocation! reloc)
    (let* ((val (dict-ref symbols (relocation-symbol reloc)))
           (src (integer->bytes val assumed-label-size #f)))
      (assert (<= (bytes-length src) assumed-label-size))
      (bytes-copy! bs (relocation-pos reloc) src)))
  (for/set ([ reloc relocs ])
    (apply-relocation! reloc)))

(: generate-relocation (-> relocation Void))
(define (generate-relocation reloc)
  (set! *relocation-table* (set-add *relocation-table* reloc)))

; Ethereum programs must have two "modules": A program to run, and an initializer that returns it.
; This prepends a minimal initializer that returns a given program.
(: wrap-loader (-> bytes bytes))
(define (wrap-loader bs)
  (let* ((len (bytes-length bs))
         (afterLoader (+ 1 (integer-bytes len) 2 2 1 1 (integer-bytes len) 2 1))
         (loader (list (eth-push 'shrink len)         ; 1 + (integer-bytes len)
                       (eth-push 1       afterLoader) ; 2 bytes
                       (eth-push 1       0)           ; 2 bytes
                       (eth-asm 'CODECOPY)            ; 1
                       (eth-push 'shrink len)         ; 1 + (integer-bytes len)
                       (eth-push 1       0)           ; 2
                       (eth-asm 'RETURN))))           ; 1
    (bytes-append (serialize loader)
                  bs)))


(: push-op? (-> opcode Boolean))
(define (push-op? op)
  (and (>= (opcode-byte op) #x60)
       (<= (opcode-byte op) #x7f)))

(: op-extra-size (-> opcode Fixnum))
(define (op-extra-size op)
  (if (push-op? op)
      (- (opcode-byte op) #x5f)
      0))
  
(: integer->hex (-> Integer String))
(define (integer->hex n)
  (bytes->hex-string (integer->bytes n assumed-label-size #f)))
      
(: instruction-size (-> EthInstruction Fixnum))
(define (instruction-size i)
  (if (eth-push? i)
      (+ 1 (eth-push-size i))
      1))

;; (remember-label (label 'derp))
;; (push-true-value (eth-push 5 'derp))
