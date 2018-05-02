#lang typed/racket

(require "utils.rkt")

(require (submod "types.rkt" common))
(require (submod "types.rkt" simulator))
(require (submod "typed.rkt" binaryio))

(provide infer-type
         parse-type)

; https://solidity.readthedocs.io/en/develop/abi-spec.html

(: parse-type (-> AbiType Bytes ContractReturnValue))
(define (parse-type type bs)
  (: assert-size (-> Integer Void))
  (define (assert-size n)
    (unless (= (bytes-length bs) n)
      (error "parse-type: Expected size" type n '!= (bytes-length bs))))
  (match type
    ["void" '()]
    ["uint256"
     (assert-size 32)
     (parse-uint256 bs)]
    ["int256"
     (assert-size 32)
     (parse-int256 bs)]
    ["uint256[]" (parse-array "uint256" bs)]
    ["bool" (= (parse-uint256 bs) 1)]
    ["bytes" bs]
    ["string" (bytes->string/utf-8 bs)]
    ["symbol" (string->symbol (integer->string (parse-uint256 bs)))]
    [_ (error "parse-type: Unsupported type:" type)]
    ))

(: type-size (-> AbiType Positive-Integer))
(define (type-size type)
  (cond ((eq? type "uint256") 32)
        (else (error "type-size: Unsupported type" type))))

(: parse-uint256 (-> Bytes EthWord))
(define (parse-uint256 bs) (truncate-int (bytes->nonnegative bs)))

(: parse-int256 (-> Bytes EthInt))
(define (parse-int256 bs) (bytes->integer bs #t))

(: parse-array (-> AbiType Bytes ContractReturnValue))
(define (parse-array type bs)
  (for/list : EthWords ([ i (in-range 0 (bytes-length bs) 32) ])
    (let ([ bs2 (subbytes bs i (+ i 32)) ])
      (parse-uint256 bs2))))

(: infer-type (-> Any AbiType))
(define (infer-type x)
  (match x
    [(? boolean?) "bool"]
    [(? exact-nonnegative-integer?) "uint256"]
    [(? exact-integer?) "int256"]
    [(? null?) "void"]
    [(? symbol?) "symbol"]
    [`(unbox ,y) (infer-type y)]
    [(? list?) (match (infer-type (car x))
                 ["uint256" "uint256[]"]
                 [t (error "infer-type: Unexpected list type" t x)])]
    [(? vector?) (match (infer-type (vector-ref x 0))
                   ["uint256" "uint256[]"]
                   [t (error "infer-type: Unexpected vector type" t x)])]
    [(? string?) "string"]
    [_ (error "infer-type: Unknown type" x)]
    ))

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
