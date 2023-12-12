(in-package :cl-user)

(defpackage :sol11
  (:use cl))

(in-package :sol11)

(load "utils.lisp")


(defun load-data (lines)
  (loop for y from 0 below (length lines)
        for line = (nth y lines)
        append (loop for x from 0 below (length line)
                     when (char= #\# (char line x))
                     collect (list x y))))

(defun expand-table (presence n)
  (loop for i from 0 to n
        with r = (make-hash-table)
        with dx = 0
        unless (gethash i presence)
          do (incf dx 999999)
        do (setf (gethash i r) dx)
        finally (return r)))



(defun expand-data (data)
  (multiple-value-bind (row column)
      (loop with hp = (make-hash-table) ;; whether there is galaxy the y line
            with vp = (make-hash-table) ;; same for the x axis
            for g in data
            for x = (first g) and y = (second g)
            with maxx = 0 and maxy = 0
            do (setf (gethash x vp) t
                     (gethash y hp) t
                     maxx (max maxx x)
                     maxy (max maxy y))
            finally (return (values (expand-table hp maxy)
                                    (expand-table vp maxx))))
    (mapcar #'(lambda (g) (list (+ (first g) (gethash (first g) column))
                                (+ (second g) (gethash (second g) row))))
            data)))


(defun coord= (a b)
  (and (= (first a) (first b))
       (= (second a) (second b))))

(defun solution ()
  (let* ((lines (load-lines "day11_input.txt"))
         (data (load-data lines))
         (expanded (expand-data data)))
    (loop with g1 = (first expanded)
          with g1-pairs = (rest expanded)
          until (null g1-pairs)
          sum (loop for g2 in g1-pairsj
                    sum (+ (abs (- (first g1) (first g2)))
                           (abs (- (second g1) (second g2)))))
          do (setf g1 (first g1-pairs)
                   g1-pairs (rest g1-pairs)))))
