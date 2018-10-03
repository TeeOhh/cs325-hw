;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 2-4789
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 2, 2018 11:21:21
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun greater (a b)
	(cond ((> a b) a)
		(t b)))

(defun has-list-p (alist)
	(cond ((null alist) nil)
		((listp (car alist)) t)
		(t (has-list-p (rest alist)))))

(defun print-dots (num)
	(do ((x 1 (1+ x)))
		((> x num) t)
		(princ ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
