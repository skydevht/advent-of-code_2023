(ql:quickload :cl-ppcre)
(load "utils.lisp")

(defun digit-cells (lines pos)
  (let* ((x (first pos))
         (y (second pos))
         (line (nth y lines))
         (startx (max (- x 1) 0))
         (starty (max (- y 1) 0))
         (endx (min (+ x 1) (- (length line) 1)))
         (endy (min (+ y 1) (- (length lines) 1))))
    ;; Looping over the 3x3 square
    (loop for ly from starty to endy
          for cl = (nth ly lines)
          append (loop for lx from startx to endx
                       for cch = (char cl lx)
                       when (and (not (and (= x lx)
                                           (= y ly)))
                                 (digit-char-p cch))
                         ;; Skipping pos
                         collect (list cch lx ly)))))

(defun number-cells (lines dcells)
  ;; returns the number associated with each digit
  ;; dcells has the form of (char x y)
  (loop for dc in dcells
        for x = (second dc)
        for y = (third dc)
        for line = (nth y lines)
        for before = (loop for lx downfrom x to 0
                           for dch = (digit-char-p (char line lx))
                           until (null dch)
                           when (not (= lx x))
                             collect dch)
        for after = (loop for lx from x to (- (length line) 1)
                          for dch = (digit-char-p (char line lx))
                          until (null dch)
                          when (not (= lx x))
                            collect dch)
        collect (list (append (reverse before)
                              (list (digit-char-p (first dc)))
                              after)
                      (list (- x (length before))
                            y))))

(defun clean-cells (ncells)
  ;; duplicates have the same coords
  ;; ncells is a list of the form (number coords)
  (remove-duplicates ncells :test #'(lambda (x y)
                                      (let ((posx (second x))
                                            (posy (second y)))
                                        (and (= (first posx) (first posy))
                                             (= (second posx) (second posy)))))))

(defun gear-ratios (lines current start)
  (if (= current (length lines))
      nil
      (let* ((line (nth current lines))
             (posx (ppcre:scan "\\*" line :start start)))
        (if (null posx)
            (gear-ratios lines (+ 1 current) 0)
            (let ((ncells (clean-cells (number-cells lines
                                                    (digit-cells lines
                                                                 (list posx current))))))
              (if (= (length ncells) 2)
                  (append (list ncells)
                          (gear-ratios lines current (+ 1 posx)))
                  (gear-ratios lines current (+ 1 posx))))))))

(defun join-digit-to-number (lst)
  (loop for d in (reverse lst)
        for i from 0
        sum (* d (expt 10 i))))


(defun solution (lines)
  (reduce #'+ (mapcar #'(lambda (star)
                          (* (join-digit-to-number (caar star))
                             (join-digit-to-number (caadr star))))
                      (gear-ratios lines 0 0))))
