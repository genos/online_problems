(in-package #:cl-user)
(defpackage #:crypto-square
  (:use #:cl)
  (:export #:encipher))
(in-package #:crypto-square)

; revised with inspiration from
; https://exercism.io/tracks/common-lisp/exercises/crypto-square/solutions/93ae7d35210e4fbdbbf754e4fc026276

(defun encipher (plaintext)
  "Construct the cipher for our plaintext"
  (let ((cleansed (clean plaintext)))
    (if (null cleansed)
      ""
      (let* ((num-cols (find-num-cols cleansed))
             (num-rows (floor (length cleansed) num-cols))
             (matrix (to-matrix cleansed num-cols))
             (transposed (transpose matrix))
             (strings (mapcar #'(lambda (row) (concatenate 'string row)) transposed)))
        (format nil "~{~A~^ ~}" strings)))))

(defun clean (text)
  "Keep only alphanumeric characters & convert to lowercase"
  (loop for c across text
        when (alphanumericp c)
        collect (char-downcase c)))

(defun find-num-cols (text)
  "Find the number of columns required for our matrix"
  (ceiling (sqrt (length text))))

(defun pad (xs num-cols)
  "Pad the list out to the specified number of columns"
  (let ((n (- num-cols (length xs))))
     (if (plusp n)
       (append xs (make-list n :initial-element #\space))
       xs)))

(defun to-matrix (xs num-cols)
  "Break the list into row-major matrix with the specified number of columns, padded out"
  (loop for ys = xs then (nthcdr num-cols ys)
        while ys
        collect (subseq (pad ys num-cols) 0 num-cols)))

(defun transpose (matrix)
  "Transpose matrix"
  ; https://rosettacode.org/wiki/Matrix_transposition#Common_Lisp
  (apply #'mapcar #'list matrix))
