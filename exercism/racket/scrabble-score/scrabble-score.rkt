#lang racket

(require racket/hash)
(provide score)


(define (score word)
  (for/sum ([c word])
    (hash-ref table (char-upcase c) 0)))

#|
Letter                           Value
A, E, I, O, U, L, N, R, S, T       1
D, G                               2
B, C, M, P                         3
F, H, V, W, Y                      4
K                                  5
J, X                               8
Q, Z                               10
|#
(define table
  (let ([t '[("AEIOULNRST". 1)
             ("DG"        . 2)
             ("BCMP"      . 3)
             ("FHVWY"     . 4)
             ("K"         . 5)
             ("JX"        . 8)
             ("QZ"        . 10)]])
    (apply hash-union
      (for/list ([pair t])
        (for/hasheqv ([c (car pair)])
          (values c (cdr pair)))))))
