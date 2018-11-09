;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: camelize-hyphenate
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: November 6, 2018 22:04:04
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------


(in-package :CS325-USER)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun camelize (string &optional capitalize)
  (if capitalize
    (remove #\- (string-capitalize string))
    (nstring-downcase (remove #\- (string-capitalize string)) :end 1)))

(defun hyphenate (string &optional (case :upper))
  (ecase case
    ((:upper) (string-upcase (with-output-to-string (s) (hyphenate-recur string s))))
    ((:lower) (string-downcase (with-output-to-string (s) (hyphenate-recur string s))))))

(defun hyphenate-recur (string stream)
  (let ((cap-position (get-cap-pos string)))
    (cond ((null string) stream)
          (cap-position (format stream "~a~a" (subseq string 0 cap-position) "-")
                        (hyphenate-recur (subseq string cap-position) stream))
          (t (format stream "~a" string)))))

(defun get-cap-pos (string)
  (let ((cap-position (position-if #'upper-case-p string :start 1)))
    (cond ((null cap-position) nil)
          ((= cap-position 1) (position-if #'lower-case-p string))
          (t cap-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
