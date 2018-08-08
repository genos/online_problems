(in-package #:cl-user)
(defpackage #:phrase
  (:use #:cl)
  (:export #:word-count))
(in-package #:phrase)

(defun word-count (s)
  (loop with h = (make-hash-table :test #'equal)
        for w in (words s)
        do (incf (gethash w h 0))
        finally (return (table-to-alist h))))

(defun clean (s)
  (remove-if-not
    #'(lambda (c) (or (alphanumericp c) (char= #\space c)))
    (string-downcase s)))

(defun split-by-one-space (s)
  "From http://cl-cookbook.sourceforge.net/strings.html"
  (loop for i = 0 then (1+ j)
        as j = (position #\Space s :start i)
        collect (subseq s i j)
        while j))

(defun words (s)
  (remove-if
    #'(lambda (s) (string= s ""))
    (split-by-one-space (clean s))))

(defun table-to-alist (h)
  (loop for k being the hash-keys in h
        collect (cons k (gethash k h))))
