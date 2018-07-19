(defpackage #:leap
  (:use #:common-lisp)
  (:export #:leap-year-p))
(in-package #:leap)

(defun leap-year-p (year)
  "Is YEAR a leap year?"
  (and
    (evenly-divisible-p year 4)
    (or
      (not (evenly-divisible-p year 100))
      (evenly-divisible-p year 400))))

(defun evenly-divisible-p (num divisor)
  "Is NUM evenly divisible by DIVISOR?"
  (zerop (rem num divisor)))
