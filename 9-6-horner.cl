;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 9-6-horner
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 25, 2018 15:35:48
;;;;   Purpose: 


(in-package :CS325-USER)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun horner (num &rest numbers)
  (do ((numbers-tracker numbers (cons (compute num (first numbers-tracker)
                                               (second numbers-tracker))
                                      (cdr (cdr numbers-tracker)))))
      ((null (cdr numbers-tracker)) (car numbers-tracker))))

(defun horner-computer (num coef1 coef2)
  (+ (* coef1 num) coef2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
