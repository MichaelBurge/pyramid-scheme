#lang racket

(require "pyramidc.rkt")
(require "globals.rkt")
(require profile)

(*verbosity* VERBOSITY-MEDIUM)
(*test?* #t)
;(profile-thunk (λ () (main "ceagle/tests/0027-chess-2.c")))
(profile-thunk (λ () (main "tests/0001-factorial.pmd")))
