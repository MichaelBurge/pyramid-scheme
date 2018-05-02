#lang typed/racket

(require typed/racket/unsafe)
(require (submod "types.rkt" simulator))
(require (submod "types.rkt" test))
(require "ast.rkt")
(require "codegen.rkt")
(require "io.rkt")
(require "expander.rkt")
(require "globals.rkt")
(require "simulator.rkt")
(require "crypto.rkt")
(require "utils.rkt")
(require "wallet.rkt")
(require "transaction.rkt")
(require "compiler.rkt")
(require "simplifier.rkt")
(require "loader.rkt")
(require racket/match)

(require (submod "typed.rkt" binaryio))

(provide (except-out (all-defined-out)
                     minicompile
                     )
         make-label
         make-label-name
         *assumed-label-size*
         label-name
         maybe->list
         expand-pyramid
         shrink-pyramid
         simplify-macros
         make-parser
         unwrap-quote
         )

; TODO: Syntax-related functions must be unsafely provided, due to https://github.com/racket/typed-racket/issues/338
(unsafe-provide minicompile
                syntax->integer
                simplify-syntax
                )

#|
This module is required into the namespace used to evaluate Pyramid macros.

Functions defined here are available to Pyramid programs within macros.

See %-install-macro-library for the namespace creation code.
|#

; Compiles a fragment of code rather than a whole program.
; The fragment doesn't have standard library or environment initialization code.
(: minicompile (-> PyramidQ EthInstructions))
(define (minicompile prog)
  (codegen-list (inst-seq-statements (compile-pyramid 'val 'next (expand-pyramid prog)))))

;; (: contains-instruction? (-> Pyramid (-> EthInstruction Boolean) Boolean))
;; (define (contains-instruction? prog pred)
;;   (let ((is (minicompile prog)))
;;     (ormap pred is)))

;; (: accesses-memory? (-> Pyramid Boolean))
;; (define (accesses-memory? exp)
;;   (define (read? i) (equal? i (evm-op 'MLOAD)))
;;   (define (write? i) (equal? i (evm-op 'MSTORE)))
;;   (define (inst-pred i) (or (read? i)
;;                             (write? i)))
;;   (contains-instruction? exp inst-pred))

;; (define (%-sig-str sig)
;;   (let ([ name (second sig) ]
;;         [ types (fourth sig) ])
;;     (string-append
;;      name
;;      "("
;;      (string-join (map symbol->string types) ",")
;;      ")")))

;; (define (%-parse-types tys)
;;   (let* ([ os 4 ]
;;          [ parse-ty (lambda ()
;;                       (let ([ ret `(parse-fixnum ,os) ])
;;                         (set! os (+ os 32))
;;                         ret))])
;;     (map parse-ty tys)))

;; (: %-register-export
;; (define (%-register-export sig)
;;   (*exports* (cons sig (*exports*))))

(module syntax-manipulators typed/racket/no-check
  (require (submod "types.rkt" ast))
  (require (submod "types.rkt" test))
  (require (submod "types.rkt" simulator))
  (require "globals.rkt")
  (require "utils.rkt")
  (require "abi.rkt")
  (require "loader.rkt")
  (require "io.rkt")
  (require "expander.rkt")
  (require "simplifier.rkt")
  (require syntax/parse)
  (require (submod "utils.rkt" syntax-parse))
  (provide (all-defined-out)
           (all-from-out 'typeable))

  (module typeable typed/racket
    (require (submod "types.rkt" ast))
    (require (submod "types.rkt" test))
    (require (submod "types.rkt" simulator))

    (require "io.rkt")
    (require "loader.rkt")
    (require "globals.rkt")
    (require "utils.rkt")
    (require "abi.rkt")
    (provide (all-defined-out))

    (: set-test-suite! (-> test-suite Void))
    (define (set-test-suite! suite)
      (*test-suite* suite))

    (: unwrap-quote (-> Sexp Sexp))
    (define (unwrap-quote x)
      (match x
        [`(quote ,y) y]
        [_ x]
    ))

    (: make-simple-test-suite (-> ContractReturnValue test-suite))
    (define (make-simple-test-suite expected)
      (test-suite
       "undefined" ; TODO: Use the currently-compiled filename
       (listof (test-case "undefined"
                 '()
                 (test-txn null null)
                 (list (test-txn (list (test-mod-assert-return expected))
                                 (list (test-expectation "assert-return" expected (make-parser expected)))))))))

    (: macro-read-file (-> String PyramidQ))
    (define (macro-read-file path)
      (read-file path #:execute? #f))

    (: make-parser (-> ContractReturnValue (-> simulation-result-ex Any)))
    (define (make-parser expected)
      (λ ([ x : simulation-result-ex ])
        (if (simulation-result? x)
            (parse-type (infer-type expected) (simulation-result-val x))
            x)))

    (: macro-include (-> String String PyramidQ))
    (define (macro-include directory filename)
      (parameterize ([ current-directory directory ])
        (macro-read-file filename)))

    (: include-unless-cached (-> String String PyramidQ))
    (define (include-unless-cached collection mod)
      (let* ([ key (string-append collection "/" mod) ]
             [ has-key? (set-member? (*required-modules*) key)]
             [ add-key! (λ () (*required-modules* (set-add (*required-modules*) key)))])
        (if has-key?
            #'(begin)
            (begin (add-key!)
                   (macro-include collection mod)
                   ))))
    )

  (require 'typeable)

  (: %-test-suite PyrMacroFunction)
  (define (%-test-suite stx)
    (syntax-parse stx
      [x:stx-test-suite (set-test-suite! (attribute x.suite-obj))])
    #'(begin))

  (: %-test-result PyrMacroFunction)
  (define (%-test-result exp)
    (define (set-value value)
      (set-test-suite! (make-simple-test-suite (syntax->datum value))))
    (syntax-case exp (box unbox)
      [(_ (box   value)) (%-test-result #'(_ value))]
      [(_ (unbox value)) (%-test-result #'(_ value))]
      [(_ (quote value)) (%-test-result #'(_ value))]
      [(_ value)         (set-value #'value)]
      )
    #'(begin))

  (define-syntax-class stx-test-suite
    #:attributes (suite-obj)
    ; TODO: Use a reference to the currently-compiled module instead of "undefined"
    [pattern ((~datum test-suite) cs:stx-test-case ...)
             #:attr suite-obj (test-suite "undefined" (attribute cs.case-obj))])

  (define-syntax-class stx-test-case
    #:datum-literals (case) #:attributes (case-obj)
    [pattern (case ~! name:string
               (~optional (accounts accs:stx-account ...))
               deploy:stx-test-deploy
               messages:stx-test-message ...)
             #:attr case-obj (test-case (datum name)
                                    (or (attribute accs.account-obj) '())
                                    (attribute deploy.txn-obj)
                                    (attribute messages.txn-obj))])

  (define-syntax-class stx-account
    #:datum-literals (quote account) #:attributes (account-obj)
    [pattern ('name:identifier balance:exact-nonnegative-integer)
             #:attr account-obj (test-account (datum name) (datum balance))])

  (define-syntax-class stx-test-deploy
    #:datum-literals (init) #:attributes (txn-obj)
    [pattern (init ~! modifiers:stx-txn-modifier ...)
             #:attr txn-obj (test-txn (attribute modifiers.mod) '())])

  (define-syntax-class stx-test-message
    #:datum-literals (txn) #:attributes (txn-obj)
    [pattern (txn ~! modifiers:stx-txn-modifier ...)
             #:attr txn-obj (test-txn (attribute modifiers.mod) '())])

  (define-syntax-class stx-txn-modifier
    #:datum-literals (quote value sender data assert-balance assert-return) #:attributes (mod)
    [pattern (value          ~! x:exact-nonnegative-integer) #:attr mod (test-mod-value          (datum x))]
    [pattern (sender         ~! 'x:identifier)               #:attr mod (test-mod-sender         (datum x))]
    [pattern (data           ~! (sender 'x:identifier))      #:attr mod (test-mod-data-sender    (datum x))]
    [pattern (assert-balance ~! 'account:identifier bal:exact-nonnegative-integer)
             #:attr mod (test-mod-assert-balance (datum account) (datum bal))]
    [pattern (assert-return  ~! x:expr)                      #:attr mod (test-mod-assert-return  (datum x))]
    )

  (: %-include PyrMacroFunction)
  (define (%-include stx)
    (syntax-case stx ()
      [(_ mod) (macro-include (*include-directory*) (syntax-e #'mod))]
      [(_ collection mod) (macro-include (get-collection-directory (syntax-e #'collection))
                                         (syntax-e #'mod))]
      ))

  (: %-require PyrMacroFunction)
  (define (%-require stx)
    (syntax-case stx ()
      [(_ mod)            (include-unless-cached (*include-directory*)
                                                 (syntax-e #'mod))]
      [(_ collection mod) (include-unless-cached (get-collection-directory (syntax-e #'collection))
                                                 (syntax-e #'mod))]
      ))
  ; Useful in macros to locally expand a fragment of syntax.
  (: simplify-syntax (-> PyramidQ PyramidQ))
  (define (simplify-syntax stx)
    (parameterize ([ *assume-macros-complete?* #t ])
      (shrink-pyramid (simplify-macros (expand-pyramid stx)))))

  (: syntax->integer (-> PyramidQ Integer))
  (define (syntax->integer stx)
    (match (syntax->datum (simplify-syntax stx))
      [`(unbox ,(? exact-integer? n)) n]
      [(? exact-integer? n) n]
      ))
  )

(require (submod 'syntax-manipulators typeable))

(unsafe-require/typed 'syntax-manipulators
  [ %-test-suite  PyrMacroFunction]
  [ %-test-result PyrMacroFunction]
  [ %-include     PyrMacroFunction]
  [ %-require     PyrMacroFunction]
  [ make-simple-test-suite (-> PyramidQ test-suite)]
  [ make-parser (-> Any (-> simulation-result-ex Any)) ]
  [ syntax->integer (-> PyramidQ Integer)]
  [ simplify-syntax (-> PyramidQ PyramidQ)]
  )

;; (define (%-selector sig) (keccak-256 (string->bytes/utf-8 (%-sig-str sig))))

; A Patchpoint consists of a symbol table entry that should be replaced with the stack output of some bytecode.
(: %-register-patchpoint! (-> Symbol EthInstructions Void))
(define (%-register-patchpoint! sym ethis)
  (define pp (patchpoint sym ethis))
  (*patchpoints* (cons pp (*patchpoints*))))

(: set-max-iterations! (-> Natural Void))
(define (set-max-iterations! x)
  (*max-simulation-steps* x)
  )

(: set-max-simulator-memory! (-> Natural Void))
(define (set-max-simulator-memory! x)
  (*simulator-memory-num-bytes* x)
  )

(define (%-install-macro-library!)
  (: base-ns Namespace)
  (define base-ns (current-namespace))
  (: attach (-> Module-Path Any))
  (define (attach m)
    (define ns (*macro-namespace*))
    (namespace-attach-module base-ns m ns)
    (parameterize ([ current-namespace ns ])
      (namespace-require m)))
  (attach "globals.rkt")
  (attach "types.rkt")
  (attach 'racket/list)
  (attach "macro.rkt")
  (attach "ast.rkt")
  (attach "utils.rkt")
  (attach 'racket/match)

  ; Debug tools
  (attach "io.rkt")
  (attach 'racket/pretty)

  (install-macro-function! 'include          %-include)
  (install-macro-function! 'require          %-require)
  (install-macro-function! 'test-suite       %-test-suite)
  (install-macro-function! 'set-test-result! %-test-result)
  )
