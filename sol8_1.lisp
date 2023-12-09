(in-package :cl-user)

(defpackage :sol8
  (:use cl))

(in-package :sol8)

(load "utils.lisp")

(defun parse-line (line)
  ;; returns a three element list (loc left right)
  (let* ((sp (uiop:split-string line :separator '(#\=)))
         (loc (string-trim " " (first sp)))
         (dest (uiop:split-string (second sp) :separator '(#\,)))
         (left (string-trim " (" (first dest)))
         (right (string-trim " )" (second dest))))
    (list loc left right)))

(defparameter +lines+ (load-lines "day8_input.txt"))

(defparameter +map+ (let ((map-hash (make-hash-table :test 'equal))
                          (map-list (mapcar #'parse-line (cddr +lines+))))
                      (loop for el in map-list
                            do (setf (gethash (first el) map-hash) (rest el)))
                      map-hash))

(defparameter +directions+ (first +lines+))

(defun solution ()
  (loop with loc = "AAA"
        with dir-i = 0
        with steps = 0
        when (string= loc "ZZZ")
          return steps
        do (setf loc
                 (nth (if (char= (char +directions+
                                       (mod dir-i (length +directions+)))
                                 #\L)
                          0
                          1)
                      (gethash loc +map+)))
        do (incf dir-i)
        do (incf steps)))
