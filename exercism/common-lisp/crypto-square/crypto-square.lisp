(in-package #:cl-user)
(defpackage #:crypto-square
  (:use #:cl)
  (:export #:encipher))
(in-package #:crypto-square)

(defun encipher (plaintext)
  (let* ((clean (remove-if-not #'alphanumericp (string-downcase plaintext)))
         (n (length clean))
         (r (isqrt n))
         (c (if (= (* r r) n) r (1+ r)))
         (cipher-cols (remove-if
                         #'(lambda (s) (string= s ""))
                         (loop
                           for i = 0 then (+ c i)
                           while (< i n)
                           collecting (subseq clean i (min (+ i c) n)))))
         (cipher-rows (loop
                        for i = 0 then (1+ i)
                        while (<= i r)
                        collecting (loop
                                      for col in cipher-cols
                                      collecting (if
                                                   (>= i (length col))
                                                   #\space
                                                   (char col i))
                                      into s
                                      finally (return (concatenate 'string s)))))
         (full (remove-if-not
                 #'(lambda (row) (some #'alphanumericp row))
                 cipher-rows)))
        (format nil "~{~A~^ ~}" full)))
