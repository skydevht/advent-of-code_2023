(in-package :cl-user)

(defpackage :sol6
  (:use cl))

(in-package :sol6)

(load "utils.lisp")

(defparameter +lines+ (load-lines "day6_input.txt"))

(defun number-line (line)
  (parse-integer (apply #'concatenate 'string
                        (mapcar #'(lambda (x) (format nil "~D" x))
                                (number-list line))) ))

(defparameter +records+ (list (number-line (first +lines+))
                              (number-line (second +lines+))))


;; the exact cuttof distance can be given by
;; the following eq -x^2 +Tx = D where T is the total time
;; and D is the total distance, (ceiling x) will give us the actual
;; cutoff

(defun cutoff (time distance)
  ;; simplified version
  (let ((delta (sqrt (- (expt time 2) (* 4 distance)))))
    (list (ceiling (/ (- time delta)
                      2))
          (floor (/ (+ time delta)
                    2)))))

(defun solution ()
  (let ((c (cutoff (first +records+) (second +records+))))
    (+ 1 (- (second c) (first c)))))
