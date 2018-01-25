#lang errortrace typed/racket/no-check

(require "types.rkt")
(require "serializer.rkt")
(require "utils.rkt")
(require "globals.rkt")
(require binaryio/integer)
(provide (all-defined-out))

(define (byte-or-zero bs i)
  (if (>= i (bytes-length bs))
      0
      (bytes-ref bs i)))

(: disassemble-one (-> bytes Fixnum (Pairof opcode EthInstruction)))
(define (disassemble-one bs i)
  (let* ([ byte (byte-or-zero bs i) ]
         [ op (dict-ref opcodes-by-byte byte (eth-unknown byte)) ]
         [ ethi (cond ((eth-unknown? op) op)
                      ((push-op? op) (disassemble-push bs i))
                      (else (eth-asm (opcode-name op)))
                      )])
    (cons op ethi)))

(: disassemble-push (-> bytes Fixnum EthInstruction))
(define (disassemble-push bs i)
  (let ([ op (dict-ref opcodes-by-byte (bytes-ref bs i)) ])
    (eth-push (op-extra-size op)
              (bytes->integer bs
                              #f      ; signed?
                              #t      ; big-endian
                              (+ i 1) ; start position
                              (+ i 1 (op-extra-size op))))) ; end
  )

; Outputs 3 column TSV suitable for pasting into Google sheets
(: print-disassembly (-> bytes Void))
(define (print-disassembly bs)
  (let ((reverse-symbol-table (invert-hash (*symbol-table*))))
    (define (loop n)
      (fprintf (current-output-port) "~x" n)
      (write-char #\tab)
      (display (reverse-symbol-name reverse-symbol-table n))
      ;; (print `(,(bytes-ref bs n)
      ;;          ,(push-op? (dict-ref opcodes-by-byte (bytes-ref bs n)))
      ;;          ,(op-extra-size (dict-ref opcodes-by-byte (bytes-ref bs n)))))
      (write-char #\tab)
      (let* ([ op-ethi (disassemble-one bs n) ]
             [ op (car op-ethi) ]
             [ ethi (cdr op-ethi) ])
        (cond ((push-op? op)
               (fprintf (current-output-port)
                        "Push~a 0x~x"
                        (op-extra-size op)
                        (eth-push-value ethi)))
              ((eth-asm? ethi) (write-string (symbol->string (opcode-name op))))
              ((eth-unknown? ethi)
               (fprintf (current-output-port)
                        "BYTE ~a"
                        (eth-unknown-byte ethi)))
              (else (error "print-disassembly: Unknown ethi" ethi)))
        (set! n (+ n (op-extra-size op))))
      (newline)
      (if (>= n (- (bytes-length bs) 1))
          (void)
          (loop (+ n 1))))
    (loop 0)))
