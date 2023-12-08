(in-package :cl-user)

(defpackage :sol6
  (:use cl))

(in-package :sol6)

(load "utils.lisp")

(defparameter +lines+ (load-lines "day6_input.txt"))

(defparameter +records+ (mapcar #'(lambda (time distance)
                                    (list time distance))
                                (number-list (first +lines+))
                                (number-list (second +lines+))))


;;;; This solution can be done much more efficiently

(defun distance (hold total)
  (* hold (- total hold)))

(defun solution ()
  (reduce #'* (loop for rec in +records+
                    collect (loop for hold from 0 to (first rec)
                                 as d = (distance hold (first rec))
                                 when (> d (second rec))
                                   sum 1))))
