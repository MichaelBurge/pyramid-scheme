#lang typed/racket/no-check

(define **exports** null)

#|
Example ABI:
(exports (uint256 (x a b c) (+ a b c)))
{
  type: "function",
  name: "x",
  inputs: 
  [
   {
    name: "a",
    type: "uint256"
    },
   {
    name: "b",
    type: "uint256"
    },
   {
    name: "c",
    type: "uint256"
    }
   ],
   outputs: [ name: "result", type: "uint256" ]
}
|#
;; (define (generate-abi sigs) (undefined))



;; (define (export-json abi) (undefined))  
