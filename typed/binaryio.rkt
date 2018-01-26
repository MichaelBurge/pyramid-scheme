#lang typed/racket

(require/typed file/sha1
  [ bytes->hex-string (-> Bytes String)]
  )

(require/typed binaryio/integer
  [ integer->bytes (-> Integer Integer Boolean Bytes)]
  [ bytes->integer (case-> (-> Bytes Boolean Integer)
                           (-> Bytes Boolean Boolean Integer)
                           (-> Bytes Boolean Boolean Integer Integer)
                           (-> Bytes Boolean Boolean Integer Integer Integer))]
  )

(provide bytes->hex-string
         integer->bytes
         bytes->integer
         )
