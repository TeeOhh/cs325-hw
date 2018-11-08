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
  (let ((hyphen-position (position #\- string)))
    (cond (hyphen-position (cond (capitalize (camelize-recur (subseq string (1+ hyphen-position))
                                                          (string-capitalize (subseq string 0 hyphen-position) :start 0 :end 1)))
                            (t (camelize-recur (subseq string (1+ hyphen-position)) (subseq string 0 hyphen-position)))))
          (capitalize (string-capitalize string :start 0 :end 1))
          (t string))))

(defun camelize-recur (string return-string)
  (let ((hyphen-position (position #\- string)))
    (cond ((null string) return-string)
          (hyphen-position (camelize-recur (subseq string (1+ hyphen-position))
                                      (concatenate 'string return-string 
                                        (string-capitalize (subseq string 0 hyphen-position) :start 0 :end 1))))
          (t (concatenate 'string return-string (string-capitalize string :start 0 :end 1))))))


(defun hyphenate (string &optional (case :upper))
  (cond ((eql case :upper) (string-upcase (hyphenate-recur string "")))
        ((eql case :lower) (string-downcase(hyphenate-recur string "")))
        (t (error "~s is not a valid argument for hyphenate. Use :upper or :lower" case))))

(defun hyphenate-recur (string return-string)
  (let ((cap-position (get-cap-pos string)))
    (cond ((null string) return-string)
          (cap-position (hyphenate-recur (subseq string cap-position)
                         (concatenate 'string return-string (subseq string 0 cap-position) "-")))
          (t (concatenate 'string return-string string)))))

(defun get-cap-pos (string)
  (let ((cap-position (position-if #'upper-case-p string :start 1)))
    (cond ((null cap-position) nil)
          ((= cap-position 1) (position-if #'lower-case-p string))
          (t cap-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
