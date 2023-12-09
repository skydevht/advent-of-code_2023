(in-package :cl-user)

(defpackage :sol7
  (:use cl))

(in-package :sol7)

(load "utils.lisp")

(defparameter +card-rank+ (list #\2 1
                                #\3 2
                                #\4 3
                                #\5 4
                                #\6 5
                                #\7 6
                                #\8 7
                                #\9 8
                                #\T 9
                                #\J 0
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
    (flet ((gettype (fst snd)
             (cond ((= fst 5) :five-kind)
                   ((= fst 4) :four-kind)
                   ((= fst 3) (if (= snd 2) :full-house :three-kind))
                   ((= fst 2) (if (= snd 2) :two-pair :one-pair))
                   (t :high-card))))
      (values (let* ((j-occ (gethash #\J hand-hash 0))
                     (fst (cond ((or (= j-occ 5) (= fst-max 5)) 5)
                                ((= j-occ 4) 5)
                                ((= j-occ 3) (if (= snd-max 2) 5 4))
                                ((= j-occ 2) (cond ((= fst-max 3) 5)
                                                   ((= snd-max 2) 4)
                                                   ((= snd-max 1) 3)))
                                ((= j-occ 1) (cond ((= fst-max 4) 5)
                                                   ((= fst-max 3) 4)
                                                   ((= fst-max 2) 3)
                                                   ((= fst-max 1) 2)))
                                (t fst-max)))
                     (snd (cond ((= j-occ 2) 1)
                                ((= j-occ 1) (if (= snd-max 2) 2 1))
                                (t snd-max))))
                (gettype fst snd))
              (gettype fst-max snd-max)))))

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
