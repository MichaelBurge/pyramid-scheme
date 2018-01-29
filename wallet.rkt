#lang typed/racket

(require "types.rkt")
(require "globals.rkt")
(require "transaction.rkt")
(require "utils.rkt")

(provide (all-defined-out))

; TODO: Remove this after properly implementing v,r,s
(: force-txn-sender! (-> vm-txn Symbol Void))
(define (force-txn-sender! txn sym)
  (set-vm-txn-r! txn (find-or-create-addr-name! sym)))

(: find-or-create-addr-name! (-> Symbol Address))
(define (find-or-create-addr-name! sym)
  (hash-ref (*addresses-by-name*)
            sym
            (λ ()
              (define addr (tick-counter! *account-nonce*))
              (register-addr-name! sym addr)
              addr)))

(: find-addr-name (-> Symbol Address))
(define (find-addr-name sym)
  (hash-ref (*addresses-by-name*) sym))

(: register-addr-name! (-> Symbol Address Void))
(define (register-addr-name! sym addr)
  (hash-set! (*addresses-by-name*) sym addr))
