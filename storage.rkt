#lang typed/racket/no-check

(require "types.rkt")
(require "globals.rkt")

(provide make-store
         store-load-checkpoint!
         store-set-account!
         store-get-value
         store-set-value!
         store-begin-txn!
         store-commit!
         )

(: make-store (-> vm-store))
(define (make-store) (vm-store (make-hash) null null))

(: store-load-checkpoint! (-> vm-store StorageRoot Void))
(define (store-load-checkpoint! store root)
  (set-vm-store-world! store (dict-ref (vm-store-history store) root (make-hash))))

(: store-set-account! (-> vm-store Address Void))
(define (store-set-account! store target)
  (set-vm-store-account! (dict-ref (vm-store-world store) target (make-hash))))

(: store-get-value (-> vm-store EthWord EthWord))
(define (store-get-value store key) (dict-ref (vm-store-account store) key))

(: store-set-value! (-> vm-store EthWord EthWord Void))
(define (store-set-value! store key value) (dict-set! (vm-store-account store) key value))

(: store-begin-txn! (-> vm-store Void))
(define (store-begin-txn! store) (void))

(: store-commit! (-> vm-store StorageRoot))
(define (store-commit!) (#xFFFFFFFF))
