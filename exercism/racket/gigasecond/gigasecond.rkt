#lang typed/racket

(provide add-gigasecond)

(require/typed racket/date
               [date->seconds (-> date Integer)])

(: add-gigasecond (-> date date))
(define (add-gigasecond datetime)
  (seconds->date (+ 1000000000 (date->seconds datetime))))
