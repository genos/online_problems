(defpackage #:collatz-conjecture
  (:use #:common-lisp)
  (:export #:collatz))

(in-package #:collatz-conjecture)

(defun collatz (n)
  (when (and (integerp n) (plusp n))
    (loop
      for k = n then (if (evenp k) (ash k -1) (+ k k k 1))
      for steps = 0 then (1+ steps)
      while (> k 1)
      finally (return steps))))
