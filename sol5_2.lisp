(in-package :cl-user)

(defpackage :sol5
  (:use cl))

(in-package :sol5)

(load "utils.lisp")

(defparameter +lines+ (load-lines "day5_input.txt"))

(defun seeds (line)
  (let ((nums (number-list line)))
    (loop for i from 0 to (- (/ (length nums) 2) 1)
          as idx = (* i 2)
          as start = (nth idx nums)
          as range = (nth (+ 1 idx) nums)
          collect (list start range))))

(defun extract-map (lines header)
  ;; Not super efficient as we start from scratch everytime
  (loop for line in lines
        as line-empty = (= (length line) 0)
        with in-map = nil
        until (and in-map line-empty)
        when in-map
          collect (number-list line)
        when (and (not in-map) (not line-empty))
          do (setf in-map (and (> (length line) (length header))
                               (string= header (subseq line 0 (length header)))))))


(defun mapping-comp-p (a b)
  (< (second a) (second b)))

;; This can be optimized
(defparameter seed-to-soil (sort (extract-map +lines+ "seed-to-soil") #'mapping-comp-p))
(defparameter soil-to-fert (sort (extract-map +lines+ "soil-to-fertilizer") #'mapping-comp-p))
(defparameter fert-to-water (sort (extract-map +lines+ "fertilizer-to-water") #'mapping-comp-p))
(defparameter water-to-light (sort (extract-map +lines+ "water-to-light") #'mapping-comp-p))
(defparameter light-to-temp (sort (extract-map +lines+ "light-to-temperature") #'mapping-comp-p))
(defparameter temp-to-hum (sort (extract-map +lines+ "temperature-to-humidity") #'mapping-comp-p))
(defparameter hum-to-loc (sort (extract-map +lines+ "humidity-to-location") #'mapping-comp-p))

(defparameter +map-list+ (list seed-to-soil
                               soil-to-fert
                               fert-to-water
                               water-to-light
                               light-to-temp
                               temp-to-hum
                               hum-to-loc))


(defun last-in-range (r)
  (+ (first r) (- (second r) 1)))

(defun interpolate (r s d)
  ;; r is our initial range, s is the boundary
  ;; d is the destination range
  ;; we assume that (first r) > (first s) is true
  (let ((endr (last-in-range r))
        (ends (last-in-range s)))
    (if (<= endr ends)
        ;; fully contained
        (values (list (+ (first d) (- (first r) (first s)))
                      (second r))
                nil)
        (values (list (+ (first d) (- (first r) (first s)))
                      (+ 1 (- ends (first r))))
                (list (+ 1 ends) (- endr ends))))))

(defun mapping (mappings start range)
  ;; this return an existing mapping from the list, or create
  ;; the non existing one that just maps the range to itself
  (loop for idx from start to (- (length mappings) 1)
        as mapping = (nth idx mappings)
        as sr = (rest mapping)
        if (and (<= (first sr) (first range))
                (<= (first range) (last-in-range sr)))
          ;; This mapping include the start of the range
          return mapping
        if (< (first range) (first sr))
          ;; There's no mapping that include the start of the range
          return (list (first range) (first range) (min (second range)
                                                        (- (first sr) (first range))))
        finally (return (list (first range) (first range) (second range)))))

(defun seed-to-loc (mappings-list current range)
  (if (= current (length mappings-list))
      (list range)
      (let* ((mappings (nth current mappings-list))
            (ranges (mapping mappings 0 range))
            (src-range (rest ranges))
            (dest-range (list (first ranges) (third ranges)))
            (result (multiple-value-list (interpolate range src-range dest-range))))
        (append (seed-to-loc mappings-list (+ 1 current) (first result))
                (if (null (second result))
                    nil
                    (seed-to-loc mappings-list current (second result)))))))

(defun get-in-map (map sr)
  (loop for line in map
        as delta = (delta (second line) (third line) n)
        when delta
          return (+ delta (first line))
        finally
           (return n)))

(defun solution ()
  (let* ((sds (seeds (first +lines+)))
        (locs (loop for seed in sds
               append (seed-to-loc +map-list+ 0 seed))))
    (apply #'min (mapcar #'first locs))))
