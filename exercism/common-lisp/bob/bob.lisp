(in-package #:cl-user)
(defpackage #:bob
  (:use #:cl)
  (:export #:response-for))
(in-package #:bob)

(defun response-for (input)
  (let ((i (string-trim '(#\Newline #\Page #\Space #\Tab) input)))
    (cond ((said-nothing-p i)                  "Fine. Be that way!")
          ((and (shouting-p i) (question-p i)) "Calm down, I know what I'm doing!")
          ((shouting-p i)                      "Whoa, chill out!")
          ((question-p i)                      "Sure.")
          (t                                   "Whatever."))))

(defun said-nothing-p (input)
  (string= "" input))

(defun question-p (input)
  (when (not (said-nothing-p input))
    (let ((last-char (char input (- (length input) 1))))
      (char= #\? last-char))))

(defun shouting-p (input)
  (when (not (said-nothing-p input))
    (and
      (some #'alpha-char-p input)
      (string= input (string-upcase input)))))
