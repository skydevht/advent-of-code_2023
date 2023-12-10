(in-package :cl-user)

(defpackage :sol10
  (:use cl))

(in-package :sol10)

(load "utils.lisp")

(defparameter +lines+ (load-lines "day10_input.txt"))

(defun symbol-at (x y)
  (char (nth y +lines+) x))

(defun coord= (a b)
  (and (= (first a) (first b))
       (= (second a) (second b))))

(defun coord-valid-p (x y)
  (and (> x 0)
       (> y 0)
       (< x (length (first +lines+)))
       (< y (length +lines+))))

(defun connected-cells (x y)
  (let ((curr (symbol-at x y)))
    (cond ((char= #\| curr) (list (list x (- y 1)) (list x (+ y 1))))
          ((char= #\- curr) (list (list (- x 1) y) (list (+ x 1) y)))
          ((char= #\L curr) (list (list x (- y 1)) (list (+ x 1) y)))
          ((char= #\J curr) (list (list x (- y 1)) (list (- x 1) y)))
          ((char= #\7 curr) (list (list (- x 1) y) (list x (+ y 1))))
          ((char= #\F curr) (list (list (+ x 1) y) (list x (+ y 1)))))))

(defun s-connected-cells (x y)
  (let  ((surr (loop for i from -1 to 1
               append (loop for j from -1 to 1
                            for nx = (+ i x) and ny = (+ j y)
                            when (not (and (= nx x) (= ny y)))
                              collect (list nx ny)))))
    (loop for cell in surr
          as possible-s = (apply #'connected-cells cell)
          when (loop for s-cand in possible-s
                     thereis (coord= s-cand (list x y)))
            collect cell)))

(defparameter +start+ (loop named l1
                            with my = (- (length +lines+) 1)
                            with mx = (- (length (first +lines+)) 1)
                            for y from 0 to my
                            for line = (nth y +lines+)
                            do (loop for x from 0 to mx
                                     for ch = (char line x)
                                     when (char= ch #\S)
                                       do (return-from l1 (list x y)))))

(defun solution ()
  (let ((path (loop with possible-start = (apply #'s-connected-cells +start+)
                    with last = +start+
                    with curr = (first possible-start)
                    collect curr into path
                    do (let ((candidates (apply #'connected-cells curr))
                             (temp curr))
                         (setf curr (if (coord= (first candidates) last)
                                        (second candidates)
                                        (first candidates))
                               last temp))
                    until (coord= curr +start+)
                    finally (return path))))
    (ceiling (/ (length path) 2))))
