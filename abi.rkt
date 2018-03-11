#lang typed/racket

(require (submod "types.rkt" common))
(require (submod "types.rkt" simulator))
(require "typed/binaryio.rkt")

(provide infer-type
         parse-type)

(: parse-type (-> AbiType Bytes Any))
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
    ["uint256[]" (parse-array "uint256" bs)]
    ["bool" (= (parse-uint256 bs) 1)]
    [_ (error "parse-type: Unsupported type:" type)]
    ))

(: type-size (-> AbiType Integer))
(define (type-size type)
  (cond ((eq? type "uint256") 32)
        (else (error "type-size: Unsupported type" type))))

(: parse-uint256 (-> Bytes EthWord))
(define (parse-uint256 bs) (bytes->integer bs #f #t))

(: parse-array (-> AbiType Bytes Any))
(define (parse-array type bs)
  (for/list : EthWords ([ i (in-range 0 (bytes-length bs) 32) ])
    (let ([ bs2 (subbytes bs i (+ i 32)) ])
      (parse-uint256 bs2))))

(: infer-type (-> Any AbiType))
(define (infer-type x)
  (cond
    [(boolean? x) "bool"]
    [(fixnum? x) "uint256"]
    [(null? x) "void"]
    [(list? x) (match (infer-type (car x))
                 ["uint256" "uint256[]"]
                 [t (error "infer-type: Unexpected list type" t)])]
    [(vector? x) (match (infer-type (vector-ref x 0))
                   ["uint256" "uint256[]"]
                   [t (error "infer-type: Unexpected vector typed" t)])]
    [else (error "infer-type: Unknown type" x)]
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
