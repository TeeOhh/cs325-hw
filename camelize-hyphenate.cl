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
  (let ((return-string (with-output-to-string (s) (hyphenate-helper string s 0))))
    (ecase case
      ((:upper) (string-upcase return-string))
      ((:lower) (string-downcase return-string)))))

(defun hyphenate-helper (string stream last-pos)
  (let ((cap-pos (get-cap-pos string last-pos)))
    (cond ((= last-pos (1- (length string))) stream)
          (cap-pos (write-string string stream :start last-pos :end cap-pos)
                   (format stream "~a" "-")
                   (hyphenate-helper string stream cap-pos))
          (t (write-string string stream :start last-pos)))))

(defun get-cap-pos (string last-pos)
  (let ((cap-position (position-if #'upper-case-p string :start (1+ last-pos))))
    (cond ((null cap-position) nil)
          ((= cap-position (1+ last-pos)) (position-if #'lower-case-p string :start (1+ last-pos)))
          (t cap-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
