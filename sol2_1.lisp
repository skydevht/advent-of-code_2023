(ql:quickload :cl-ppcre)

(defun max-ball (k)
  (cond ((eq k :red) 12)
        ((eq k :green) 13)
        ((eq k :blue) 14)))

(defun load-lines (filename)
  (with-open-file (stream (merge-pathnames filename))
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun game-ball (str)
  (let ((d (uiop:split-string str)))
    (list (intern (string-upcase (second d)) "KEYWORD")
          (parse-integer (first d)))))

(defun game-set (str)
  (reduce #'append (mapcar #'(lambda (x)
               (game-ball (string-trim " " x)))
           (uiop:split-string str :separator '(#\,)))))

(defun game (line)
  (let ((d (uiop:split-string line :separator '(#\: #\;))))
    (list :game (parse-integer (second (uiop:split-string (first d))))
          :sets (mapcar #'game-set (rest d)))))

(defun game-set-p (set)
  (loop for s in set
        always (loop for k in '(:red :green :blue)
                    for v = (getf s k)
                    never (and (not (null v))
                               (> v (max-ball k))))))

(defun game-p (game)
  (game-set-p (getf game :sets)))

(defun solution (lines)
  (loop for line in lines
        for g = (game line)
        when (game-p g)
          sum (getf g :game)))
