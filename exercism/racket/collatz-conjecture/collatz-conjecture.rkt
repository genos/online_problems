#lang typed/racket

(provide collatz)

(: collatz (-> Positive-Integer Nonnegative-Integer))
(define (collatz n)
  (let loop ([i : Nonnegative-Integer 0]
             [k  n])
    (if (= 1 k)
      i
      (loop (add1 i)
            (if (even? k)
              (arithmetic-shift k -1)
              (add1 (* 3 k)))))))
