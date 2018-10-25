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
  (cond ((eql (length numbers) 1) (* (first numbers) num))
        ((null numbers) 0)
        (t (+ (* (first numbers) num) (second numbers))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
