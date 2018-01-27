#lang typed/racket

(require/typed file/sha1
  [ bytes->hex-string (-> Bytes String)]
  )

(require/typed binaryio/integer
  [ integer->bytes (case-> (-> Integer Integer Boolean Bytes) ; n size signed?
                           (-> Integer Integer Boolean Boolean Bytes))] ; n size signed? big-endian?
  [ bytes->integer (case-> (-> Bytes Boolean Integer)
                           (-> Bytes Boolean Boolean Integer)
                           (-> Bytes Boolean Boolean Integer Integer)
                           (-> Bytes Boolean Boolean Integer Integer Integer))]
  )

(provide bytes->hex-string
         integer->bytes
         bytes->integer
         )
