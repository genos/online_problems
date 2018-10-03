(defpackage #:collatz-conjecture
  (:use #:common-lisp)
  (:export #:collatz))

(in-package #:collatz-conjecture)

(defun collatz (n)
  (when (and (integerp n) (plusp n))
    (loop
      for k = n then (if (evenp k) (/ k 2) (1+ (* 3 k)))
      while (> k 1)
      counting k)))
