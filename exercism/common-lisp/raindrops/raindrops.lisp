(defpackage #:raindrops
  (:use #:common-lisp)
  (:export #:convert))

(in-package #:raindrops)

(defun convert (n)
  (flet ((droplet (modulus sound)
           (if (zerop (mod n modulus))
             sound
             "")))
    (let* ((droplets
             (list (droplet 3 "Pling")
                   (droplet 5 "Plang")
                   (droplet 7 "Plong")))
           (stringified (apply #'concatenate 'string droplets)))
      (if (string= "" stringified)
        (format nil "~A" n)
        stringified))))
