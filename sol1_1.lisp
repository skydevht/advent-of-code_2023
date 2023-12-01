(defun load-lines (filename)
  (with-open-file (stream (merge-pathnames filename))
    (loop for line = (read-line stream nil nil)
          while line
            collect line)))

(defun first-digit (line)
  (let ((digit (find-if #'digit-char-p line)))
    (if digit (digit-char-p digit) nil)))

(defun last-digit (line)
  (let ((digit (find-if #'digit-char-p line :from-end t)))
    (if digit (digit-char-p digit) nil)))

(defun line-to-number (line)
  (let ((fd (first-digit line))
        (ld (last-digit line)))
    (+ (* fd 10)
       ld)))

(defun solution (lines)
  (reduce #'+ (mapcar #'line-to-number lines)))

(solution (load-lines "day1_input.txt"))
