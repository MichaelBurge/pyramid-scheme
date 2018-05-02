#lang typed/racket

(require "utils.rkt")
(require "types.rkt")
(require (submod "types.rkt" common))
(require (submod "types.rkt" test))
(require (submod "types.rkt" evm-assembly))
(require (submod "types.rkt" simulator))
(require (submod "types.rkt" abstract-machine))
(require (submod "types.rkt" pyramidc))
(require (submod "types.rkt" ast))

(provide (all-defined-out))

(define VERBOSITY-NONE 0)
(define VERBOSITY-LOW 1)
(define VERBOSITY-MEDIUM 2)
(define VERBOSITY-HIGH 3)
(define VERBOSITY-ALL 4)

; All shared global variables should be moved here

(: *verbosity* (Parameterof Fixnum))
(define *verbosity* (make-parameter 0))

(: verbose? (-> Verbosity Boolean))
(define (verbose? n) (>= (*verbosity*) n))

(: *test?* (Parameterof Boolean))
(define *test?* (make-parameter #f))

(: *minimize?* (Parameterof Boolean))
(define *minimize?* (make-parameter #f))

(: *simplify?* (Parameterof Boolean))
(define *simplify?* (make-parameter #t))
; (define optimize-level (make-parameter 0))

(: *warnings?* (Parameterof Boolean))
(define *warnings?* (make-parameter #t))

(: *link?* (Parameterof Boolean))
(define *link?* (make-parameter #t))

(: *use-debug-symbols?* (Parameterof Boolean))
(define *use-debug-symbols?* (make-parameter #f))

(: *include-directory* (Parameterof String))
(define *include-directory* (make-parameter "."))

(: *test-suite* (Parameterof test-suite))
(define *test-suite* (make-parameter (test-suite "undefined" '())))

(: *exports* (Parameterof (Listof Any)))
(define *exports* (make-parameter null)) ; Used to generate the standard ABI for the current Pyramid contract

(: *loader-size* (Parameterof Natural))
(define *loader-size* (make-parameter 0)) ; Size of the most recently generated loader

(: *byte-offset* (Parameterof UnlinkedOffset))
(define *byte-offset* (make-parameter 0)) ; Used during serialization to track output stream position

(: *abstract-offset* (Parameterof Natural))
(define *abstract-offset* (make-parameter 0)) ; Used to generate debug labels. Index of the last-generated abstract machine instruction.

(: *symbol-table* (Parameterof SymbolTable))
(define *symbol-table* (make-parameter (make-symbol-table)))

(: *relocation-table* (Parameterof RelocationTable))
(define *relocation-table* (make-parameter (make-relocation-table)))

(: *reverse-symbol-table* (Parameter ReverseSymbolTable))
(define *reverse-symbol-table* (make-parameter (make-reverse-symbol-table)))

(: *available-macros* (Parameterof Namespace))
(define *available-macros* (make-parameter (make-empty-namespace)))

(: *macro-namespace* (Parameterof Namespace))
(define *macro-namespace* (make-parameter (make-base-namespace)))

(: *asm-namespace* (Parameterof Namespace))
(define *asm-namespace* (make-parameter (make-base-namespace)))

(: *patchpoints* (Parameterof (Listof patchpoint)))
(define *patchpoints* (make-parameter null))

(: *required-modules* (Parameterof (Setof Any)))
(define *required-modules* (make-parameter (set)))

(: *addresses-by-name* (Parameterof AddressTable))
(define *addresses-by-name* (make-parameter (make-address-table)))

(: *txn-nonce* (Parameterof Natural))
(define *txn-nonce* (make-parameter 0))

(: *account-nonce* (Parameterof Natural))
(define *account-nonce* (make-parameter 100))

; TODO: Contract address should use actual Ethereum spec rather than a counter
(: *contract-create-counter* (Parameterof Natural))
(define *contract-create-counter* (make-parameter 200))

(: *label-counter* (Parameterof Natural))
(define *label-counter* (make-parameter 0))

(: *primops* (Parameterof PrimopTable))
(define *primops*
  (let ([ tbl : PrimopTable (make-hash)])
    (make-parameter tbl)))

(: *recursion-depth* (Parameterof Natural))
(define *recursion-depth* (make-parameter 10))

(: *evm-source-map-stack* (Parameterof (Listof Symbol)))
(define *evm-source-map-stack* (make-parameter null))

(: *evm-source-map* (Parameterof SourceMap))
(define *evm-source-map* (make-parameter (make-source-map)))

(: on-simulate-nop (-> vm-exec EthInstruction EthWords Void))
(define (on-simulate-nop vm i reads) (void))
(define *on-simulate-instruction* (make-parameter on-simulate-nop))
(define *on-simulate-error* (make-parameter on-simulate-nop))

(: on-log-nop (-> vm-exec Bytes EthWords Void))
(define (on-log-nop vm bs tags) (void))

(: *on-log* (Parameterof (-> vm-exec Bytes EthWords Void)))
(define *on-log* (make-parameter on-log-nop))

(: *max-simulation-steps* (Parameterof Natural))
(define *max-simulation-steps* (make-parameter 1000000))

(: *compile-time-lexicals* (Parameterof LexicalTable))
(define *compile-time-lexicals* (make-parameter (make-lexical-table)))

(: *expander-options* (Parameterof expander-options))
(define *expander-options* (make-parameter (expander-options #t)))

(: *simulator-memory-num-bytes* (Parameter Natural))
(define *simulator-memory-num-bytes* (make-parameter 2000000))

(: *assume-macros-complete?* (Parameterof Boolean))
(define *assume-macros-complete?* (make-parameter #f))

; Constants
(define assumed-label-size 3) ; TODO: Number of bytes to leave behind for label relocations. This makes it difficult to write programs larger than 65536 bytes.
(define *assumed-label-size* assumed-label-size)
(define DEFAULT-GAS-PRICE 10)
(define DEFAULT-GAS-LIMIT 1000000)
(define ALLOCATION-RANGE-PADDING 1000) ; Length of gaps between abstract analyzer's address space allocations. Catches most out-of-range writes.
(define WORD #x20) ; 256-bit words / 8 bit granularity addresses = 32 8-bit words, or 0x20.
(define IOCTL-TAG 0)

; Syntax properties
(define STXPROP-OUT-OPTIONS 'out-options)
