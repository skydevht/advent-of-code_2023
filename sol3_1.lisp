(ql:quickload :cl-ppcre)
(load "utils.lisp")


(defun dot-p (line pos)
  (char= #\. (char line pos)))

(defun part-number-p (frame surr)
  (let* ((line (second surr))
         (start (max (- (first frame) 1) 0))
         (end (min (+ 1 (second frame)) (length line))))
    (or (not (loop for y in '(0 2)
                   for cline = (nth y surr)
                   always (if (null cline)
                              t
                              (loop for x from start to (- end 1)
                                    always  (dot-p cline x)))))
        (not (if (= (first frame) start)
                 t
                 (dot-p line start)))
        (not (if (= (second frame) (length line))
                 t
                 (dot-p line (- end 1)))))))


(defun surr-lines (lines current)
  (let ((fst (if (= current 0)
                 nil
                 (nth (- current 1) lines)))
        (snd (nth current lines))
        (trd (if (= current (- (length lines) 1))
                 nil
                 (nth (+ 1 current) lines))))
    (list fst snd trd)))

(defun line-parts (lines current start)
  (if (= current (length lines))
      nil
      (let ((pos (multiple-value-list (ppcre:scan "\\d+"
                                                  (nth current lines)
                                                  :start start))))
        (cond ((null (first pos)) (line-parts lines (+ 1 current) 0))
              (t (append (if (part-number-p pos (surr-lines lines current))
                             (list (parse-integer (subseq (nth current lines)
                                                          (first pos)
                                                          (second pos))))
                             nil)
                         (line-parts lines current (second pos))))))))


(defun solution (lines)
  (reduce #'+ (line-parts lines 0 0)))
