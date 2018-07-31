#lang typed/racket

(provide perfect-numbers)
(require math)

(define-type ℕ Natural)

(: perfect-numbers (-> ℕ (Listof ℕ)))
(define (perfect-numbers n)
  (filter perfect? (range 1 (add1 n))))

(: perfect? (-> ℕ Boolean))
(define (perfect? n)
  (let ([ds (drop-right (divisors n) 1)])
    (= (apply + ds) n)))
