#lang typed/racket

(provide leap-year?)

;; version 1
(: leap-year? (-> Integer Boolean))
(define (leap-year? year)
  (and (divides? year 4)
       (or (not (divides? year 100))
           (divides? year 400))))

#|
;; version 2
(: leap-year? (-> Integer Boolean))
(define (leap-year? year)
  (foldl
    (lambda ([d : Integer] [acc : Boolean])
      (cast (xor acc (divides? year d)) Boolean))
    #f
    '(4 100 400)))

;; version 3
(: leap-year? (-> Integer Boolean))
(define (leap-year? year)
  (match (map (lambda ([ d : Integer ]) (divides? year d))
              '(4 100 400))
    ['(#t #f #f) #t]
    ['(#t #t #f) #f]
    ['(#t #t #t) #t]
    [_ #f]))
|#

(: divides? (-> Integer Integer Boolean))
(define (divides? number divisor)
  (zero? (remainder number divisor)))
