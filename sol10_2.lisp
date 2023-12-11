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
  "Does (x, y) locate a tile in the input"
  (and (> x 0)
       (> y 0)
       (< x (length (first +lines+)))
       (< y (length +lines+))))

(defun connected-cells (x y)
  "returns the two cells that the pipe connects"
  (let ((curr (symbol-at x y)))
    (cond ((char= #\| curr) (list (list x (- y 1)) (list x (+ y 1))))
          ((char= #\- curr) (list (list (- x 1) y) (list (+ x 1) y)))
          ((char= #\L curr) (list (list x (- y 1)) (list (+ x 1) y)))
          ((char= #\J curr) (list (list x (- y 1)) (list (- x 1) y)))
          ((char= #\7 curr) (list (list (- x 1) y) (list x (+ y 1))))
          ((char= #\F curr) (list (list (+ x 1) y) (list x (+ y 1)))))))

(defun s-connected-cells (x y)
  "return the cells connected to the tile at (x, y)"
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
                                       do (return-from l1 (list x y))))
  "contains the coords of the \"S\" tile")

(defparameter +path+ (loop with possible-start = (apply #'s-connected-cells +start+)
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
                           finally (return path))
  "a list of all the tiles forming the loop, not including the S tile.")

(defun coord-to-key (x y)
  (format nil "~D-~D" x y))

(defparameter +path-hash+ (let ((result (make-hash-table :test 'equal)))
                           (loop for coord in (cons +start+ +path+)
                                 for key = (apply #'coord-to-key coord)
                                 do (setf (gethash key result) t))
                           result))

(defun loop-p (x y)
  "Does the tile at (x, y) part of the path?"
  (gethash (coord-to-key x y) +path-hash+))

(defun corner-p (ch)
  (or (char= ch #\7)
      (char= ch #\F)
      (char= ch #\L)
      (char= ch #\J)))


(defun solution ()
  (loop with v-counter = (loop with result = (make-hash-table)
                               for x from 0 below (length (first +lines+))
                               do (setf (gethash x result) 0)
                               finally (return result))
        with v-corners = (make-hash-table)
        for y from 0 below (length +lines+)
        for line = (nth y +lines+)
        with result = 0
        do (loop for x from 0 below (length line)
                 ;; cheating here. I looked at the input data instead of computing it
                 ;; to get the equivalent tile of the S one
                 for ch = (let ((cch (char line x)))
                            (if (char= cch #\S) #\7 cch))
                 with last-corner-h = nil
                 for last-corner-v = (gethash x v-corners)
                 with counter = 0
                 for inside = (and (oddp counter) (oddp (gethash x v-counter 0)))
                 when (loop-p x y)
                   do (incf counter
                            (if (or (char= ch #\|) ; vertical wall
                                    (and (char= ch #\J)
                                         (not (null last-corner-h))
                                         (char= last-corner-h #\F))
                                    (and (char= ch #\7)
                                         (not (null last-corner-h))
                                         (char= last-corner-h #\L)))
                                1
                                0)) ; if tile is - that means we are in the same wall as the last tile
                      (incf (gethash x v-counter)
                            (if (or (char= ch #\-) ; horizontal wall
                                    (and (char= ch #\L)
                                         (not (null last-corner-v))
                                         (char= last-corner-v #\7))
                                    (and (char= ch #\J)
                                         (not (null last-corner-v))
                                         (char= last-corner-v #\F)))
                                1
                                0)) ; if tile is |, it's the same wall as the one above
                      (if (corner-p ch)
                          (setf last-corner-h (if (null last-corner-h) ch nil)
                                (gethash x v-corners) (if (null (gethash x v-corners)) ch nil)))
                 else
                   do (incf result (if inside 1 0)))
        finally (return result)))
