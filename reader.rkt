#lang racket

(provide (all-defined-out))

(define (read-integer c ip filename line col pos)
  (case c
    [(#\u) #`(unbox #,(read-syntax/recursive filename ip))]
    [(#\b) #`(box   #,(read-syntax/recursive filename ip))]
    ))

(define (read-dot c ip filename line col pos)
  (define tail (read-syntax/recursive filename ip))
  (case c
    [(#\.) #`(%#dot #,tail)]
    ))

(define (make-pyramid-readtable)
  (make-readtable (current-readtable)
                  ;#\. 'dispatch-macro read-dot
                  #\u 'dispatch-macro read-integer
                  #\b 'dispatch-macro read-integer
                  ))

;(: read-pyramid-syntaxes (-> Path Input-Port (Listof (Syntaxof PyramidQ))))
(define (read-pyramid-syntaxes path fh)
  (parameterize ([ current-readtable (make-pyramid-readtable) ])
    ;(: loop (-> (Listof (Syntaxof PyramidQ))))
    (define (loop)
      (let ([ x (read-syntax path fh) ])
        ;(displayln x)
        ;(displayln (continuation-mark-set-first (current-continuation-marks) '(line)))
        ;(displayln (continuation-mark-set->list* (current-continuation-marks) '(line)))
        (if (eof-object? x)
            null
            (cons x (loop)))))
    (loop))
  )
