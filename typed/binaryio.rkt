#lang typed/racket

(require/typed file/sha1
  [ bytes->hex-string (-> Bytes String)]
  [ hex-string->bytes (-> String Bytes)]
  )

(require/typed binaryio/integer
  [ integer->bytes (case-> (-> Integer Integer Boolean Bytes) ; n size signed?
                           (-> Integer Integer Boolean Boolean Bytes))] ; n size signed? big-endian?
  [ bytes->integer (case-> (-> Bytes Boolean Integer)
                           (-> Bytes Boolean Boolean Integer)
                           (-> Bytes Boolean Boolean Integer Integer)
                           (-> Bytes Boolean Boolean Integer Integer Integer))] ; bs signed? big-endian? start end
  )

(provide bytes->hex-string
         hex-string->bytes
         integer->bytes
         bytes->integer
         )
