#lang typed/racket

(provide score)

(: score (-> String Natural))
(define (score word)
  (for/sum : Natural ([c word])
    (case (char-upcase c)
      [(#\A #\E #\I #\O #\U #\L #\N #\R #\S #\T) 1]
      [(#\D #\G) 2]
      [(#\B #\C #\M #\P) 3]
      [(#\F #\H #\V #\W #\Y) 4]
      [(#\K) 5]
      [(#\J #\X) 8]
      [(#\Q #\Z) 10]
      [else 0])))
