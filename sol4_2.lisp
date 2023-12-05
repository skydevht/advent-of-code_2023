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

(defun count-match (card)
  (let ((winning (make-hash-table)))
    (loop for n in (second card)
          do (setf (gethash n winning) 0))
    (loop for n in (third card)
          do (multiple-value-bind (count present-p) (gethash n winning)
               (if present-p
                   (setf (gethash n winning) (+ 1 count))))
          )
    (let ((match 0))
      (maphash #'(lambda (k v) (incf match v)) winning)
      match)))

(defun count-scratchpad (lines start end)
  (loop for i from start to end
        for line = (nth i lines)
        sum (let ((count (count-match (card line))))
              (if (= count 0)
                  1
                  (+  1 (count-scratchpad lines (+ 1 i) (+ count i)))))))

(defun solution (lines)
  (count-scratchpad lines 0 (- (length lines) 1)))
