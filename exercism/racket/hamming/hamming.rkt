#lang typed/racket

(provide hamming-distance)

(: hamming-distance (-> String String Natural))
(define (hamming-distance a b)
  (if (= (string-length a) (string-length b))
    (for/fold ([d : Natural 0])
              ([x (in-string a)]
               [y (in-string b)]
               #:when (not (char=? x y)))
      (add1 d))
    (error 'lengths-differ)))
