#lang typed/racket

(require (submod "types.rkt" simulator))
(require "globals.rkt")
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
              (: addr Address)
              (define addr (tick-counter! *account-nonce*))
              (register-addr-name! sym addr)
              addr)))

(: find-name (-> Symbol Address))
(define (find-name sym)
  (hash-ref (*addresses-by-name*) sym))

(: find-addr-name (-> Address (U Symbol #f)))
(define (find-addr-name addr)
  (: kv (U #f (Pairof Symbol Address)))
  (define kv (findf (λ ([ p : (Pairof Symbol Address) ]) (equal? addr (cdr p)))
                    (hash->list (*addresses-by-name*))))
  (if kv
      (car kv)
      #f))

(: register-addr-name! (-> Symbol Address Void))
(define (register-addr-name! sym addr)
  (hash-set! (*addresses-by-name*) sym addr))
