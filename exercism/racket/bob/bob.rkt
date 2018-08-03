#lang typed/racket

(provide response-for)

(: response-for (-> String String))
(define (response-for str)
  (let ([s : String (string-normalize-spaces str)])
    (cond
      [(said-nothing? s) "Fine. Be that way!"]
      [(shouting? s)     "Whoa, chill out!"]
      [(question? s)     "Sure."]
      [else              "Whatever."])))

(: said-nothing? (-> String Boolean))
(define (said-nothing? s)
  (string=? s ""))

(: shouting? (-> String Boolean))
(define (shouting? s)
  (and
    (regexp-match? #rx"[A-Z]" s)
    (for/and ([c : Char (in-string s)])
      (if (char-alphabetic? c)
        (char-upper-case? c)
        #true))))

(: question? (-> String Boolean))
(define (question? s)
  (char=? (string-ref s (sub1 (string-length s))) #\?))
