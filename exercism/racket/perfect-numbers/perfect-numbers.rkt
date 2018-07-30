#lang typed/racket

(provide perfect-numbers)

(define-type ℕ Natural)

(: perfect-numbers (-> ℕ (Listof ℕ)))
(define (perfect-numbers n)
  (filter perfect? (range 1 (add1 n))))

(: perfect? (-> ℕ Boolean))
(define (perfect? n)
  (let ([sum-of-factors
          (for/fold ([sum : ℕ 0])
                    ([i (in-range 1 n)] #:when (zero? (remainder n i)))
            (cast (+ sum i) ℕ))])
    (= sum-of-factors n)))
