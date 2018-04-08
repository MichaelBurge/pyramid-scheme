#lang typed/racket

(require (submod "types.rkt" common))
(require (submod "types.rkt" simulator))
(require "globals.rkt")
(require "utils.rkt")

(provide make-txn-create
         make-txn-message
         txn-sender
         )

(: make-txn-create (-> Bytes vm-txn))
(define (make-txn-create bytecode)
  (vm-txn (alloc-txn-nonce) ; nonce
          DEFAULT-GAS-PRICE ; gas price
          DEFAULT-GAS-LIMIT ; gas limit
          #f                ; to
          0                 ; value
          28                ; v
          0                 ; r
          0                 ; s
          bytecode          ; input
          ))

(: make-txn-message (-> Address EthWord Bytes vm-txn))
(define (make-txn-message to value input)
  (when (null? to)
    (error "make-txn-message: 'to' cannot be null"))
  (vm-txn (alloc-txn-nonce) ; nonce
          DEFAULT-GAS-PRICE ; gas price
          DEFAULT-GAS-LIMIT ; gas limit
          to                ; to
          value             ; value
          28                ; v
          0                 ; r
          0                 ; s
          input             ; input
          ))

(: alloc-txn-nonce (-> Natural ))
(define (alloc-txn-nonce) (tick-counter! *txn-nonce*))

; TODO: Deduce this from v,r,s
(: txn-sender (-> vm-txn Address))
(define (txn-sender txn)
  (: r Address)
  (define r (vm-txn-r txn))
  (if (> r 0)
      r
      (+ 1234 (vm-txn-nonce txn))))
