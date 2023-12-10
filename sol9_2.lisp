(in-package :cl-user)

(defpackage :sol9
  (:use cl))

(in-package :sol9)

(load "utils.lisp")

(defparameter +lines+ (load-lines "day9_input.txt"))

(defparameter +data+ (mapcar #'number-list +lines+))

(defun interpret (history)
  (let ((new-hist (loop for i from 0 to (- (length history) 2)
               for a = (nth i history) and b = (nth (+ i 1) history)
               collect (- b a))))
    (if (loop for el in new-hist
              always (= el 0))
        (first history)
        (- (first history) (interpret new-hist)))))

(defun solution ()
  (loop for history in +data+
        sum (interpret history)))
