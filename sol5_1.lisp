(in-package :cl-user)

(defpackage :sol5
  (:use cl))

(in-package :sol5)

(load "utils.lisp")

(defun seeds (line)
  (number-list line))

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



(defun delta (start range n)
  (let ((end (+ (- range 1) start)))
    (if (or (> n end) (< n start))
        nil
        (- n start))))

(defun get-in-map (map n)
  (loop for line in map
        as delta = (delta (second line) (third line) n)
        when delta
          return (+ delta (first line))
        finally
           (return n)))

(defun solution (lines)
  (let ((sds (seeds (first lines))))
    (apply #'min (loop with seed-to-soil = (extract-map lines "seed-to-soil")
                       with soil-to-fert = (extract-map lines "soil-to-fertilizer")
                       with fert-to-water = (extract-map lines "fertilizer-to-water")
                       with water-to-light = (extract-map lines "water-to-light")
                       with light-to-temp = (extract-map lines "light-to-temperature")
                       with temp-to-hum = (extract-map lines "temperature-to-humidity")
                       with hum-to-loc = (extract-map lines "humidity-to-location")
                       for seed in sds
                       as soil = (get-in-map seed-to-soil seed)
                       as fert = (get-in-map soil-to-fert soil)
                       as water = (get-in-map fert-to-water fert)
                       as light = (get-in-map water-to-light water)
                       as temp = (get-in-map light-to-temp light)
                       as hum = (get-in-map temp-to-hum temp)
                       as loc = (get-in-map hum-to-loc hum)
                       collect  loc))))
