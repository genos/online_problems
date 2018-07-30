(in-package #:cl-user)
(defpackage #:bob
  (:use #:cl)
  (:export #:response-for))
(in-package #:bob)

(defun response-for (input)
  (let ((i (trim-whitespace input)))
    (cond ((empty-p i)                         "Fine. Be that way!")
          ((and (shouting-p i) (question-p i)) "Calm down, I know what I'm doing!")
          ((shouting-p i)                      "Whoa, chill out!")
          ((question-p i)                      "Sure.")
          (t                                   "Whatever."))))

(defun trim-whitespace (input)
  (let ((whitespace '(#\Newline #\Page #\Space #\Tab)))
    (string-trim whitespace input)))

(defun last-char (input)
  (if (empty-p input)
    nil
    (elt input (- (length input) 1))))

(defun empty-p (input)
  (string= "" input))

(defun non-empty-p (input)
  (not (empty-p input)))

(defun question-p (input)
  (char= #\? (last-char input)))

(defun shouting-p (input)
  (and
    (some #'alpha-char-p input)
    (string= input (string-upcase input))))
