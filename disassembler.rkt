#lang errortrace typed/racket

(require "types.rkt")
(require "serializer.rkt")
(require "utils.rkt")
(require "globals.rkt")

(require "typed/binaryio.rkt")
(require "typed/dict.rkt")
(provide (all-defined-out))

(: byte-or-zero (-> Bytes Integer Byte))
(define (byte-or-zero bs i)
  (if (>= i (bytes-length bs))
      0
      (bytes-ref bs i)))

(: disassemble-one (-> Bytes Integer EthInstruction))
(define (disassemble-one bs i)
  (let* ([ byte (byte-or-zero bs i) ])
    (if (hash-has-key? opcodes-by-byte byte)
        (disassemble-opcode bs i (hash-ref opcodes-by-byte byte))
        (eth-unknown byte))))

(: disassemble-opcode (-> Bytes Integer opcode EthInstruction))
(define (disassemble-opcode bs i op)
  (cond ((push-op? op) (disassemble-push bs i))
        (else          (eth-asm (opcode-name op)))
        ))

   
(: disassemble-push (-> Bytes Integer EthInstruction))
(define (disassemble-push bs i)
  (let ([ op (hash-ref opcodes-by-byte (bytes-ref bs i)) ])
    (eth-push (op-extra-size op)
              (bytes->integer bs
                              #f      ; signed?
                              #t      ; big-endian
                              (+ i 1) ; start position
                              (+ i 1 (op-extra-size op)))))) ; end

; Outputs 3 column TSV suitable for pasting into Google sheets
(: print-disassembly (-> Bytes Void))
(define (print-disassembly bs)
  (let ((reverse-symbol-table (invert-hash (*symbol-table*))))
    (: loop (-> Integer Void))
    (define (loop n)
      (fprintf (current-output-port) "~x" n)
      (write-char #\tab)
      (display (reverse-symbol-name reverse-symbol-table n))
      ;; (print `(,(bytes-ref bs n)
      ;;          ,(push-op? (hash-ref opcodes-by-byte (bytes-ref bs n)))
      ;;          ,(op-extra-size (hash-ref opcodes-by-byte (bytes-ref bs n)))))
      (write-char #\tab)
      (let ([ ethi (disassemble-one bs n) ])
        (cond ((eth-push? ethi)
               (begin
                 (define op (ethi->opcode ethi))
                 (fprintf (current-output-port)
                          "Push~a 0x~x"
                          (op-extra-size op)
                          (eth-push-value ethi))))
              ((eth-asm? ethi) (write-string (symbol->string (opcode-name (ethi->opcode ethi)))))
              ((eth-unknown? ethi)
               (fprintf (current-output-port)
                        "BYTE ~a"
                        (eth-unknown-byte ethi)))
              (else (error "print-disassembly: Unknown ethi" ethi)))
        (set! n (+ n (ethi-extra-size ethi))))
      (newline)
      (if (>= n (- (bytes-length bs) 1))
          (void)
          (loop (+ n 1))))
    (loop 0)))
