(defun m (n)
  "ProgPraxis's sieve solution to
  'How Many Distinct Products in a Time Table?'"
  (let ((bits (make-array (1+ (* n n))
                          :element-type 'bit
                          :adjustable nil)))
    (loop for i from 1 to n do
          (loop for j from (* i i) to (* i n) by i
                do (setf (elt bits j) 1)))
    (loop for b across bits summing b)))
