(ql:quickload :alexandria)
(ql:quickload :fset)

;;; naïve

(defconstant +input+ (mapcar #'parse-integer (uiop:read-file-lines #p"input.txt")))

; works just fine for the size of our input
(defun part-1-naive ()
  (loop :for (x y)
        :in (alexandria:map-product 'list +input+ +input+)
        :when (= 2020 (+ x y))
        :return (* x y)))

; chokes & dies, as well it should
(defun part-2-naive ()
  (loop :for (x y z)
        :in (alexandria:map-product 'list +input+ +input+ +input+)
        :when (= 2020 (+ x y z))
        :return (* x y z)))

;;; smarter

(defconstant +input-set+ (fset:convert 'fset:set +input+))

(defun find-pair (goal)
  (alexandria:when-let ((x (fset:find-if (lambda (x)
                                           (fset:contains? +input-set+ (- goal x)))
                                         +input-set+)))
    (list x (- goal x))))

(defun find-triple (goal)
  (alexandria:when-let ((x (fset:find-if (lambda (x) (find-pair (- goal x))) +input-set+)))
    (cons x (find-pair (- goal x)))))

(defun part-1 ()
  (apply #'* (find-pair 2020)))

(defun part-2 ()
  (apply #'* (find-triple 2020)))
