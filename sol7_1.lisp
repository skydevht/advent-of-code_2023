(in-package :cl-user)

(defpackage :sol7
  (:use cl))

(in-package :sol7)

(load "utils.lisp")

(defparameter +card-rank+ (list #\2 0
                                #\3 1
                                #\4 2
                                #\5 3
                                #\6 4
                                #\7 5
                                #\8 6
                                #\9 7
                                #\T 8
                                #\J 9
                                #\Q 10
                                #\K 11
                                #\A 12))

(defun card< (a b)
  ;; return true if the card a
  ;; is ranked less than the card b
  (< (getf +card-rank+ a)
     (getf +card-rank+ b)))

(defun card= (a b)
  ;; return true if the card a
  ;; has the same rank as the card b
  (= (getf +card-rank+ a)
     (getf +card-rank+ b)))

(defun card<= (a b)
  (or (card< a b) (card= a b)))

(defun card> (a b)
  (not (card<= a b)))

(defparameter +type-rank+ (list :five-kind 6
                                :four-kind 5
                                :full-house 4
                                :three-kind 3
                                :two-pair 2
                                :one-pair 1
                                :high-card 0))

(defun type< (a b)
  ;; return true if the type of hand a is ranked
  ;; less than the type of hand b
  (< (getf +type-rank+ a)
     (getf +type-rank+ b)))

(defun type> (a b)
  ;; return true if the type of hand a is ranked
  ;; less than the type of hand b
  (> (getf +type-rank+ a)
     (getf +type-rank+ b)))



(defun hand-type (hand)
  (let ((hand-hash (make-hash-table))
        (fst-max 0)
        (snd-max 0))
    ;; compute occurences
    (loop for ch across (the simple-string hand)
          for count = (gethash ch hand-hash 0)
          do (setf (gethash ch hand-hash) (+ 1 count)))
    ;; find the highest and second highest occurences
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (if (<= v fst-max)
                     (setf snd-max (max v snd-max))
                     (setf snd-max fst-max))
                 (setf fst-max (max v fst-max)))
             hand-hash)
    (cond ((= fst-max 5) :five-kind)
          ((= fst-max 4) :four-kind)
          ((= fst-max 3) (if (= snd-max 2) :full-house :three-kind))
          ((= fst-max 2) (if (= snd-max 2) :two-pair :one-pair))
          (t :high-card))))

(defun hand< (a b)
  (cond ((type< (hand-type a) (hand-type b)) t)
        ((type> (hand-type a) (hand-type b)) nil)
        ;; type should be equal at this point
        (t (loop for card-a across (the simple-string a)
                 for card-b across (the simple-string b)
                 if (card< card-a card-b)
                   return t
                 if (card> card-a card-b)
                   return nil
                 finally (return nil)))))

(defparameter +lines+ (load-lines "day7_input.txt"))

(defun bid (line)
  (let ((split (uiop:split-string line)))
    (list (first split) (parse-integer (second split)))))

(defun bid< (a b)
  (hand< (first a) (first b)))

(defun solution ()
  (loop with sorted-bids = (stable-sort (mapcar #'bid +lines+)
                                        #'bid<)
        for rank from 1 to (length sorted-bids)
        for bid = (nth (- rank 1) sorted-bids)
        sum (* rank (second bid))))
