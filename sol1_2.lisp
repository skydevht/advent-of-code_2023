(ql:quickload :cl-ppcre)
(defparameter +FREG+ "one|two|three|four|five|six|seven|eight|nine")
(defparameter +LREG+ (reverse +FREG+))

(defun load-lines (filename)
  (with-open-file (stream (merge-pathnames filename))
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun scan-pos (regex line)
  (multiple-value-bind (start end) (cl-ppcre:scan regex line)
    (list start end)))

(defun ord-num (ord)
  (cond ((string= ord "one") 1)
        ((string= ord "two") 2)
        ((string= ord "three") 3)
        ((string= ord "four") 4)
        ((string= ord "five") 5)
        ((string= ord "six") 6)
        ((string= ord "seven") 7)
        ((string= ord "eight") 8)
        ((string= ord "nine") 9)))

(defun first-digit (line)
  (let ((ord-pos (scan-pos +freg+ line))
        (num-pos (scan-pos "\\d" line)))
    (cond ((and (not (null (first ord-pos)))
                (not (null (first num-pos))))
           (if (< (first ord-pos)
                  (first num-pos))
               (ord-num (apply #'subseq line ord-pos))
               (digit-char-p (char line (first num-pos)))))
          ((not (null (first ord-pos)))
           (ord-num (apply #'subseq line ord-pos)))
          ((not (null (first num-pos)))
           (digit-char-p (char line (first num-pos)))))))

(defun last-digit (line)
  (let* ((rline (reverse line))
         (ord-pos (scan-pos +lreg+ rline))
         (num-pos (scan-pos "\\d" rline)))
    (cond ((and (not (null (first ord-pos)))
                (not (null (first num-pos))))
           (if (< (first ord-pos)
                  (first num-pos))
               (ord-num (reverse (apply #'subseq rline ord-pos)))
               (digit-char-p (char rline (first num-pos)))))
          ((not (null (first ord-pos)))
           (ord-num (reverse (apply #'subseq rline ord-pos))))
          ((not (null (first num-pos)))
           (digit-char-p (char rline (first num-pos)))))))


(defun line-to-number (line)
  (let ((fd (first-digit line))
        (ld (last-digit line)))
    (+ (* fd 10)
       ld)))

(defun solution (lines)
  (reduce #'+ (mapcar #'line-to-number lines)))

(solution (load-lines "day1_input.txt"))
