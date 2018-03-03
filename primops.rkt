#lang typed/racket

(require (submod "types.rkt" abstract-machine))
(require "codegen.rkt")
(require "abstract-analyzer.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(: make-primops (-> (Listof primop)))
(define (make-primops)
  (listof
   ;        Name                      EVM Assembly Generator          Abstract Interpreter
   ; No-ops: These are compiled to nothing, but have semantic meaning
   [primop 'bool->unboxed             cg-identity                     eval-op-bool->unboxed]
   ; Variables
   [primop 'define-variable!          op-define-variable!             eval-op-define-variable!]
   [primop 'set-variable-value!       op-set-variable-value!          eval-op-set-variable-value!]
   [primop 'lookup-variable-value     op-lookup-variable-value        eval-op-lookup-variable-value]
   [primop 'extend-environment        cg-op-extend-environment        eval-op-extend-environment]
   ; Runtime
   [primop 'type                      cg-tag                          eval-op-tag]
   [primop 'allocate                  cg-allocate                     eval-op-allocate]
   [primop 'read-memory               cg-read-address-offset          eval-op-read-memory]
   [primop 'write-memory              cg-write-address-offset         eval-op-write-memory]
   ; Booleans
   [primop 'false?                    cg-op-false?                    eval-op-false?]
   ; Fixnums
   [primop 'make-fixnum               cg-make-fixnum                  eval-op-make-fixnum]
   [primop 'fixnum-value              cg-unbox-integer                eval-op-fixnum-value]
   [primop 'add                       cg-add                          eval-op-add]
   ; Continuations
   [primop 'save-continuation         cg-save-continuation            eval-op-save-continuation]
   [primop 'restore-continuation!     op-restore-continuation         eval-op-restore-continuation]
   [primop 'continuation?             op-continuation?                eval-op-continuation?]
   ; Compiled Procedures
   [primop 'make-compiled-procedure   cg-op-make-compiled-procedure   eval-op-make-compiled-procedure]
   [primop 'compiled-procedure-entry  cg-op-compiled-procedure-entry  eval-op-compiled-procedure-entry]
   [primop 'compiled-procedure-env    cg-op-compiled-procedure-env    eval-op-compiled-procedure-env]
   [primop 'compiled-procedure?       op-compiled-procedure?          eval-op-compiled-procedure?]
   ; Primitive Procedures
   [primop 'primitive-procedure?      cg-op-primitive-procedure?      eval-op-primitive-procedure?]
   [primop 'apply-primitive-procedure cg-op-apply-primitive-procedure eval-op-apply-primitive-procedure]
   ; Lists
   [primop 'singleton                 cg-op-list                      eval-op-singleton]
   [primop 'pair                      cg-op-cons                      eval-op-pair]
   [primop 'pair?                     cg-pair?                        eval-op-pair?]
   [primop 'left                      cg-car                          eval-op-left]
   [primop 'right                     cg-cdr                          eval-op-right]
   [primop 'null?                     cg-null?                        eval-op-null?]
   [primop 'null                      cg-make-nil                     eval-op-make-null]
   ))

(: make-primop-table (-> PrimopTable))
(define (make-primop-table)
  (make-hash (map (λ ([p : primop]) (cons (primop-name p) p))
                  (make-primops))))