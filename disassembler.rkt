#lang typed/racket

(require (submod "types.rkt" common))
(require (submod "types.rkt" evm-assembly))
(require "serializer.rkt")
(require "utils.rkt")
(require "globals.rkt")

(require (submod "typed.rkt" binaryio))
(require (submod "typed.rkt" dict))
(provide (all-defined-out))

(: disassemble-one (-> Bytes Natural EthInstruction))
(define (disassemble-one bs i)
  (let* ([ byte (byte-or-zero bs i)])
    (if (hash-has-key? opcodes-by-byte byte)
        (disassemble-opcode bs i (hash-ref opcodes-by-byte byte))
        (evm-bytes (bytes byte)))))

(: disassemble-opcode (-> Bytes Integer opcode EthInstruction))
(define (disassemble-opcode bs i op)
  (cond ((push-op? op) (disassemble-push bs i))
        (else          (evm-op (opcode-name op)))
        ))


(: disassemble-push (-> Bytes Integer EthInstruction))
(define (disassemble-push bs i)
  (let ([ op (hash-ref opcodes-by-byte (bytes-ref bs i)) ])
    (evm-push (op-extra-size op)
              (bytes->integer bs
                              #f      ; signed?
                              #t      ; big-endian
                              (+ i 1) ; start position
                              (+ i 1 (op-extra-size op)))))) ; end

; Outputs 3 column TSV
(: print-disassembly (-> Bytes Void))
(define (print-disassembly bs)
  (define end (- (bytes-length bs) 1))
  (let ((reverse-symbol-table (invert-hash (*symbol-table*))))
    (: loop (-> Natural Void))
    (define (loop n)
      (if (>= n end)
          (void)
          (begin
            (printf "~x" n)
            (write-char #\tab)
            (display (reverse-symbol-name reverse-symbol-table (- n (*loader-size*))))
            ;; (print `(,(bytes-ref bs n)
            ;;          ,(push-op? (hash-ref opcodes-by-byte (bytes-ref bs n)))
            ;;          ,(op-extra-size (hash-ref opcodes-by-byte (bytes-ref bs n)))))
            (write-char #\tab)
            (let ([ ethi (disassemble-one bs n) ])
              (match ethi
                [(struct evm-push  (size value)) (printf "Push ~a 0x~x" size value )]
                [(struct evm-op    (sym)       ) (write-string (symbol->string sym))]
                [(struct evm-bytes (bs)        ) (printf "BYTES ~a" bs             )]
                [_ (error "print-disassembly: Unknown ethi" ethi                   )])
              (newline)
              (loop (+ n (instruction-size ethi)))))))
    (loop 0)))
