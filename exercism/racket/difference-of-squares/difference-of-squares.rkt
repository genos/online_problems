#lang typed/racket

(provide sum-of-squares square-of-sums difference)

(define-type ℕ Natural)

(: sum-of-squares (-> ℕ ℕ))
(define (sum-of-squares n)
  ;; square pyramidal numbers
  (cast (* 1/6 n (+ 1 n) (+ 1 n n)) ℕ))

(: square-of-sums (-> ℕ ℕ))
(define (square-of-sums n)
  ;; triangular numbers
  (let ((sum (cast (* 1/2 n (+ 1 n)) ℕ)))
    (* sum sum)))

(: difference (-> ℕ ℕ))
(define (difference n)
  (cast (- (square-of-sums n) (sum-of-squares n)) ℕ))
