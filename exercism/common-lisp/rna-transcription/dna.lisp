(in-package #:cl-user)
(defpackage #:dna
  (:use #:cl)
  (:export #:to-rna))
(in-package #:dna)

(defun transform-dna-char (c)
  "Transform a DNA character (one of {A, C, G, T}) into its RNA counterpart"
  (declare (standard-char c)
           (optimize (speed 3) (safety 0)))
  (the standard-char
       (ccase c
              (#\G #\C)
              (#\C #\G)
              (#\T #\A)
              (#\A #\U))))

(defun to-rna (str)
  "Transcribe a string representing DNA nucleotides to RNA."
  (declare (string str)
           (optimize (speed 3) (safety 0)))
  (the string (map 'string #'transform-dna-char str)))
