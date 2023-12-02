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

(defun min-balls (set)
  (loop for s in set
        maximize (getf s :red 1) into red
        maximize (getf s :green 1) into green
        maximize (getf s :blue 1) into blue
        finally (return (list red green blue))))

(defun set-power (set)
  (apply #'* (min-balls set)))

(defun solution (lines)
  (reduce #'+ (mapcar #'(lambda (x)
                          (set-power (getf (game x)
                                           :sets)))
                      lines)))

(solution (load-lines "day2_input.txt"))
