#lang typed/racket

(require "types.rkt")
(require "typed/binaryio.rkt")

(provide infer-type
         parse-type)

(: parse-type (-> AbiType Bytes Any))
(define (parse-type type bs)
  (cond ((equal? type "void") '())
        ((equal? type "uint256")   (parse-uint256 bs))
        ((equal? type "uint256[]") (parse-array "uint256" bs))
        (else (error "parse-pyramid-result: Unsupported type:" type))))

(: type-size (-> AbiType Integer))
(define (type-size type)
  (cond ((eq? type "uint256") 32)
        (else (error "type-size: Unsupported type" type))))

(: parse-uint256 (-> Bytes EthWord))
(define (parse-uint256 bs) (bytes->integer bs #f #t))

(: parse-array (-> AbiType Bytes Any))
(define (parse-array type bs)
  (let ([ ret null ])
    (for ([ i (in-range 0 (bytes-length bs) 32) ])
      (let ([ bs2 (subbytes bs i (+ i 32)) ])
        (set! ret (cons (parse-uint256 bs2) ret))))
    ret))

(: infer-type (-> Any AbiType))
(define (infer-type x)
  (cond ((fixnum? x) "uint256")
        ((null? x) "void")
        ((list? x) (match (infer-type (car x))
                     ("uint256" "uint256[]")
                     (t (error "unexpected list type" t))))
        (else (error "infer-type: Unknown type" x))))

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
