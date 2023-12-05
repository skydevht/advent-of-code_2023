(load "utils.lisp")


(defun card-number (card-str)
  (parse-integer (second (uiop:split-string card-str :max 2))))

(defun number-list (str)
  (loop for i in (mapcar #'(lambda (x) (parse-integer x :junk-allowed t))
           (uiop:split-string str))
        when i
          collect i))

(defun card (line)
  (let* ((fsplit (uiop:split-string line :separator '(#\:)))
         (ssplit (uiop:split-string (second fsplit) :separator '(#\|))))
    (list (card-number (first fsplit))
          (number-list (first ssplit))
          (number-list (second ssplit)))))

(defun count-win (card)
  (let ((winning (make-hash-table)))
    (loop for n in (second card)
          do (setf (gethash n winning) 1))
    (loop for n in (third card)
          with points = 0
          do (multiple-value-bind (count present-p) (gethash n winning)
               (if present-p
                   (setf points (if (= points 0)
                                    1
                                    (* 2 points)))))
          finally (return points))))

(defun solution (lines)
  (reduce #'+ (mapcar #'(lambda (l) (count-win (card l))) lines)))
