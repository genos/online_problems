(in-package #:cl-user)
(defpackage #:robot
  (:use #:common-lisp)
  (:export #:build-robot #:robot-name #:reset-name))

(in-package #:robot)

;; Initialize pseudorandomness
(setf *random-state* (make-random-state t))

(defparameter
  *seen-names*
  (make-hash-table :test #'equal)
  "Efficiently stored collection of all previously seen names")

(defun names-exhausted-p ()
  "Are all possible robot names used?"
  (>= (hash-table-size *seen-names*)
      (* 26 26 10 10 10))) ;; two uppercase characters + three digits

(defun rand-char ()
  "Generate a random uppercase ASCII character"
  (elt "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (random 26)))

(defun rand-digit ()
  "Generate a single random digit"
  (elt "0123456789" (random 10)))

(defun rand-name ()
  "Generate a random name: two characters and three digits"
  (coerce
    (list (rand-char) (rand-char) (rand-digit) (rand-digit) (rand-digit))
    'string))

(defun new-name ()
  "Generate a _new_ random name, one not already in *seen-names*"
  (if (names-exhausted-p)
    (error "All possible robot names are in use.")
    (loop
      for name = (rand-name) then (rand-name)
      until (not (gethash name *seen-names*))
      finally (progn
                (setf (gethash name *seen-names*) (the bit 0))
                (return name)))))

(defstruct robot
  "An automaton with a name"
  name)

(defun build-robot ()
  "Create a new robot"
  (make-robot :name (new-name)))

(defun reset-name (r)
  "Change the supplied robot's name to a new random one"
  (setf (robot-name r) (new-name)))
