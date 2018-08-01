#lang typed/racket

(provide add-gigasecond)

(require threading)
(require/typed racket/date
               [date->seconds (-> date Integer)])

(: add-gigasecond (-> date date))
(define (add-gigasecond d)
  (~> d
      date->seconds
      (+ 1000000000)
      seconds->date))
