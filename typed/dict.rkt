#lang typed/racket

(require/typed racket/dict
  [ dict->list (All (A B) (-> (HashTable A B) (Listof (Pairof A B))))]
  [ dict-ref (All (A B) (case-> (-> (HashTable A B) A B) (-> (HashTable A B) A B B)))]
  [ dict-set! (All (A B) (-> (HashTable A B) A B Void))]
  )

(provide dict->list
         dict-ref
         dict-set!)
