(defun load-lines (filename)
  (with-open-file (stream (merge-pathnames filename))
    (loop for line = (read-line stream nil nil)
          while line
            collect line)))
