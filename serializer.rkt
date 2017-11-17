#lang typed/racket/no-check

(require "types.rkt")
(require file/sha1)

; Global variables
(define byte-offset 0)

(: serialize-print (-> EthInstructions Void))
(define (serialize-print is)
  (write (bytes->hex-string (serialize is))))

(: serialize (-> EthInstructions bytes))
(define (serialize is)
  (if (null? is)
      (bytes)
      (bytes-append (serialize-one (car is))
                    (serialize     (cdr is)))))

(: serialize-one (-> EthInstruction bytes))
(define (serialize-one i)
  (cond ((eth-unknown? i) (bytes i))
        (else
         (error "Unknown EthInstruction - serialize-one:" i))))
(define newline (bytes 10))
