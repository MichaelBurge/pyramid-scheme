#lang racket

(require "pyramidc.rkt")
(require "globals.rkt")
(require profile)

(*verbosity* VERBOSITY-NONE)
(*test?* #t)
(profile-thunk (λ () (main "ceagle/tests/0027-chess-2.c")))
