(in-package #:cl-user)
(defpackage #:acronym
  (:use #:common-lisp)
  (:export #:acronym))
(in-package #:acronym)

(defun acronym (s)
  (if (string= s "")
    ""
    (string-upcase (coerce (initials s) 'string))))

; With help from http://cl-cookbook.sourceforge.net/strings.html
(defun initials (s)
  (loop for i = 0 then (1+ j)
        as j = (position-if
                 #'(lambda (c) (or (char= c #\space) (char= c #\-)))
                 s :start i)
        collect (char s i)
        while j))
