#lang typed/racket

(require/typed racket/set
  [ set-add! (All (A) (-> (Setof A) A Void))])

(provide set-add!)
