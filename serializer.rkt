#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "utils.rkt")
(require "globals.rkt")
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

(: opcode-table (Listof opcode))
(define opcode-table
  ; Byte Symbol StackReads StackWrites
  `(,(opcode #x00 'STOP       0 0 )
    ,(opcode #x01 'ADD        2 1 )
    ,(opcode #x02 'MUL        2 1)
    ,(opcode #x03 'SUB        2 1)
    ,(opcode #x04 'DIV        2 1)
    ,(opcode #x05 'SDIV       2 1)
    ,(opcode #x06 'MOD        2 1 )
    ,(opcode #x07 'SMOD       2 1)
    ,(opcode #x08 'ADDMOD     3 1)
    ,(opcode #x09 'MULMOD     2 1)
    ,(opcode #x0a 'EXP        2 1)
    ,(opcode #x0b 'SIGNEXTEND 2 1)

    ,(opcode #x10 'LT         2 1)
    ,(opcode #x11 'GT         2 1)
    ,(opcode #x12 'SLT        2 1)
    ,(opcode #x13 'LGT        2 1)
    ,(opcode #x14 'EQ         2 1)
    ,(opcode #x15 'ISZERO     1 1)
    ,(opcode #x16 'AND        2 1)
    ,(opcode #x17 'OR         2 1)
    ,(opcode #x18 'XOR        2 1)
    ,(opcode #x19 'NOT        1 1)
    ,(opcode #x1a 'BYTE       2 1)
    
    ,(opcode #x20 'SHA3       2 1)
    
    ,(opcode #x30 'ADDRESS      0 1)
    ,(opcode #x31 'BALANCE      1 1)
    ,(opcode #x32 'ORIGIN       0 1)
    ,(opcode #x33 'CALLER       0 1)
    ,(opcode #x34 'CALLVALUE    0 1)
    ,(opcode #x35 'CALLDATALOAD 1 1)
    ,(opcode #x36 'CALLDATASIZE 0 1)
    ,(opcode #x37 'CALLDATACOPY 3 1)
    ,(opcode #x38 'CODESIZE     0 1)
    ,(opcode #x39 'CODECOPY     3 0)
    ,(opcode #x3a 'GASPRICE     0 1)
    ,(opcode #x3b 'EXTCODESIZE  1 1)
    ,(opcode #x3c 'EXTCODECOPY  4 0)
    
    ,(opcode #x40 'BLOCKHASH    1 1)
    ,(opcode #x41 'COINBASE     0 1)
    ,(opcode #x42 'TIMESTAMP    0 1)
    ,(opcode #x43 'NUMBER       0 1)
    ,(opcode #x44 'DIFFICULTY   0 1)
    ,(opcode #x45 'GASLIMIT     0 1)
    
    ,(opcode #x50 'POP          1 0)
    ,(opcode #x51 'MLOAD        1 1)
    ,(opcode #x52 'MSTORE       2 0)
    ,(opcode #x53 'MSTORE8      2 0)
    ,(opcode #x54 'SLOAD        1 1)
    ,(opcode #x55 'SSTORE       2 0)
    ,(opcode #x56 'JUMP         1 0)
    ,(opcode #x57 'JUMPI        2 0)
    ,(opcode #x58 'PC           0 1)
    ,(opcode #x59 'MSIZE        0 1)
    ,(opcode #x5a 'GAS          0 1)
    ,(opcode #x5b 'JUMPDEST     0 0)
    
    ,(opcode #x60 'PUSH1        0 1)
    ,(opcode #x61 'PUSH2        0 1)
    ,(opcode #x62 'PUSH3        0 1)
    ,(opcode #x63 'PUSH4        0 1)
    ,(opcode #x64 'PUSH5        0 1)
    ,(opcode #x65 'PUSH6        0 1)
    ,(opcode #x66 'PUSH7        0 1)
    ,(opcode #x67 'PUSH8        0 1)
    ,(opcode #x68 'PUSH9        0 1)
    ,(opcode #x69 'PUSH10       0 1)
    ,(opcode #x6a 'PUSH11       0 1)
    ,(opcode #x6b 'PUSH12       0 1)
    ,(opcode #x6c 'PUSH13       0 1)
    ,(opcode #x6d 'PUSH14       0 1)
    ,(opcode #x6e 'PUSH15       0 1)
    ,(opcode #x6f 'PUSH16       0 1)
    ,(opcode #x70 'PUSH17       0 1)
    ,(opcode #x71 'PUSH18       0 1)
    ,(opcode #x72 'PUSH19       0 1)
    ,(opcode #x73 'PUSH20       0 1)
    ,(opcode #x74 'PUSH21       0 1)
    ,(opcode #x75 'PUSH22       0 1)
    ,(opcode #x76 'PUSH23       0 1)
    ,(opcode #x77 'PUSH24       0 1)
    ,(opcode #x78 'PUSH25       0 1)
    ,(opcode #x79 'PUSH26       0 1)
    ,(opcode #x7a 'PUSH27       0 1)
    ,(opcode #x7b 'PUSH28       0 1)
    ,(opcode #x7c 'PUSH29       0 1)
    ,(opcode #x7d 'PUSH30       0 1)
    ,(opcode #x7e 'PUSH31       0 1)
    ,(opcode #x7f 'PUSH32       0 1)
    
    ,(opcode #x80 'DUP1         1 2)
    ,(opcode #x81 'DUP2         2 3)
    ,(opcode #x82 'DUP3         3 4)
    ,(opcode #x83 'DUP4         4 5)
    ,(opcode #x84 'DUP5         5 6)
    ,(opcode #x85 'DUP6         6 7)
    ,(opcode #x86 'DUP7         7 8)
    ,(opcode #x87 'DUP8         8 9)
    ,(opcode #x88 'DUP9         9 10)
    ,(opcode #x89 'DUP10        10 11)
    ,(opcode #x8a 'DUP11        11 12)
    ,(opcode #x8b 'DUP12        12 13)
    ,(opcode #x8c 'DUP13        13 14)
    ,(opcode #x8d 'DUP14        14 15)
    ,(opcode #x8e 'DUP15        15 16)
    ,(opcode #x8f 'DUP16        16 17)
    
    ,(opcode #x90 'SWAP1        2 2)
    ,(opcode #x91 'SWAP2        3 3)
    ,(opcode #x92 'SWAP3        4 4)
    ,(opcode #x93 'SWAP4        5 5)
    ,(opcode #x94 'SWAP5        6 6)
    ,(opcode #x95 'SWAP6        7 7)
    ,(opcode #x96 'SWAP7        8 8)
    ,(opcode #x97 'SWAP8        9 9)
    ,(opcode #x98 'SWAP9        10 10)
    ,(opcode #x99 'SWAP10       11 11)
    ,(opcode #x9a 'SWAP11       12 12)
    ,(opcode #x9b 'SWAP12       13 13)
    ,(opcode #x9c 'SWAP13       14 14)
    ,(opcode #x9d 'SWAP14       15 15)
    ,(opcode #x9e 'SWAP15       16 16)
    ,(opcode #x9f 'SWAP16       17 17)
    
    ,(opcode #xa0 'LOG0 2 0)
    ,(opcode #xa1 'LOG1 3 0)
    ,(opcode #xa2 'LOG2 4 0)
    ,(opcode #xa3 'LOG3 5 0)
    ,(opcode #xa4 'LOG3 6 0)
    
    ,(opcode #xf0 'CREATE       3 1)
    ,(opcode #xf1 'CALL         7 1)
    ,(opcode #xf2 'CALLCODE     7 1)
    ,(opcode #xf3 'RETURN       2 0)
    ,(opcode #xf4 'DELEGATECALL 6 1)
    
    ,(opcode #xfd 'REVERT       0 0)
    ,(opcode #xfe 'INVALID      0 0)
    ,(opcode #xff 'SUICIDE      1 0)
    ))

(define opcodes-by-sym
  (make-hash (map (lambda (op)
                    (cons (opcode-name op) op))
                  opcode-table)))

(define opcodes-by-byte
  (make-hash (map (lambda (op)
                    (cons (opcode-byte op) op))
                  opcode-table)))

(: serialize-with-relocations (-> EthInstructions bytes))
(define (serialize-with-relocations is)
  (let* ((bs (serialize is)))
    (apply-relocations! bs (*relocation-table*) (*symbol-table*))
    (*reverse-symbol-table* (invert-dict (*symbol-table*)))
    bs)
  )

; Consider using wrap-loader to prepend an initializer program.
(: serialize (-> EthInstructions bytes))
(define (serialize is)
  (parameterize ([ *byte-offset* 0 ])
    (apply bytes-append (map serialize-one is))
  ))
  ;; (if (null? is)
  ;;     (bytes)
  ;;     (bytes-append (serialize-one (car is))
  ;;                   (serialize     (cdr is)))))

(: serialize-one (-> EthInstruction bytes))
(define (serialize-one i)
  (*byte-offset* (+ 1 (*byte-offset*)))
  (cond ((eth-asm?     i) (serialize-asm i))
        ((eth-push?    i) (serialize-push i))
        ((eth-unknown? i) (bytes (eth-unknown-byte i)))
        ((label?       i) (serialize-label i))
        (else
         (error "Unknown EthInstruction - serialize-one:" i))))

(: serialize-asm (-> eth-asm bytes))
(define (serialize-asm i)
  (serialize-opcode (eth-to-opcode i)))

(: eth-to-opcode (-> EthInstruction opcode))
(define (eth-to-opcode ethi)
  (cond ((eth-asm? ethi) (lookup-opcode (eth-asm-name ethi)))
        ((eth-push? ethi) (lookup-push-opcode ethi))
        ((eth-unknown? ethi) (error "eth-to-opcode: Attempted to look up eth-unknown opcode" ethi))
        ((label? ethi) (lookup-opcode 'JUMPDEST))
        (else (error "eth-to-opcode: Unknown case"))))

(: serialize-opcode (-> opcode bytes))
(define (serialize-opcode op)
  (bytes (opcode-byte op)))

(: serialize-push (-> eth-push bytes))
(define (serialize-push push)
  (let ((op (lookup-push-opcode push))
        (val (push-true-value push))
        (size (push-true-size push)))
    (*byte-offset* (+ size (*byte-offset*)))
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
  (dict-set! (*symbol-table*) lbl (- (*byte-offset*) 1)))

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
                          (generate-relocation (relocation (*byte-offset*) val))
                          (dict-ref (*symbol-table*) (label-name val) 0)))
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
  (*relocation-table* (set-add (*relocation-table*) reloc)))

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
    (*loader-size* afterLoader)
    (bytes-append (serialize loader)
                  bs)))


(: push-op? (-> opcode Boolean))
(define (push-op? op)
  (and
   (opcode? op)
   (>= (opcode-byte op) #x60)
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

(: reset-serializer-globals! (-> Void))
(define (reset-serializer-globals!)
  (*byte-offset* 0)
  (*symbol-table* (make-hash '()))
  (*relocation-table* (set)))

;; (remember-label (label 'derp))
;; (push-true-value (eth-push 5 'derp))
