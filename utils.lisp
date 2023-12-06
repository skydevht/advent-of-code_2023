(defun load-lines (filename)
  (with-open-file (stream (merge-pathnames filename))
    (loop for line = (read-line stream nil nil)
          while line
            collect line)))

(defun line-id (str)
  (parse-integer (second (uiop:split-string str :max 2))))

(defun number-list (str)
  (loop for i in (mapcar #'(lambda (x) (parse-integer x :junk-allowed t))
           (uiop:split-string str))
        when i
          collect i))
