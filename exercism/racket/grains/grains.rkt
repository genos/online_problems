#lang typed/racket

(provide square total)

(: square (-> Natural Natural))
(define (square n)
  (cast (expt 2 (- n 1)) Natural))

(: total (-> Natural))
(define (total)
  (cast (sub1 (square 65)) Natural))
