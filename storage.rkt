#lang typed/racket/no-check

(require "types.rkt")
(require "globals.rkt")
(require "io.rkt")

(provide make-store
         store-load-checkpoint!
         store-set-account!
         store-get-value
         store-set-value!
         store-begin-txn!
         store-commit!
         store-get-root
         )

(define HARDCODED-STORAGE-ROOT #xFFFFFFFF)

(: make-store (-> vm-store))
(define (make-store) (vm-store null null (make-hash)))

(: store-load-checkpoint! (-> vm-store StorageRoot Void))
(define (store-load-checkpoint! store root) (void))

(: store-set-account! (-> vm-store Address Void))
(define (store-set-account! store target) (void))

(: store-get-value (-> vm-store EthWord EthWord))
(define (store-get-value store key) (dict-ref (vm-store-account store) key 0))

(: store-set-value! (-> vm-store EthWord EthWord Void))
(define (store-set-value! store key value) (dict-set! (vm-store-account store) key value))

(: store-begin-txn! (-> vm-store Void))
(define (store-begin-txn! store) (void))

(: store-commit! (-> vm-store StorageRoot))
(define (store-commit! store) HARDCODED-STORAGE-ROOT)

(: store-get-root (-> vm-store StorageRoot))
(define (store-get-root store) HARDCODED-STORAGE-ROOT)
