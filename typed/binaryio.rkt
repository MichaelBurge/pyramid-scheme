#lang typed/racket

(require/typed file/sha1
  [ bytes->hex-string (-> Bytes String)]
  )

(require/typed binaryio/integer
  [ integer->bytes (-> Integer Integer Boolean Bytes)]
  )

(provide bytes->hex-string
         integer->bytes
         )
