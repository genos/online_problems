(in-package #:cl-user)
(defpackage #:gigasecond
  (:use #:cl)
  (:export #:from))
(in-package #:gigasecond)

(defun from (year month day hour minute second)
  (nthcdr 3
    (reverse
      (multiple-value-list
        (decode-universal-time
          (+ 1000000000
            (encode-universal-time second minute hour day month year 0))
          0)))))
