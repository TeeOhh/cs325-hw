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

(defun print-dots (num)
	(princ (print-dots-helper num "")))

(defun print-dots-helper (num init)
	(cond ((= num 0) init)
		  (t (print-dots-helper (1- num) (concatenate 'string "." init)))))

(defun get-a-count (alist)
	(apply #'+ (mapcar (lambda (x) (if (eql x 'a) 1 0)) alist)))

(defun get-a-count (alist)
	(cond ((null alist) 0)
		  ((eql (first alist) 'a) (1+ (get-a-count (rest alist))))
		  (t (get-a-count (rest alist)))))

(defun summit (alist)
	(apply #'+ (mapcar (lambda (x) (if x x 0)) alist)))

(defun summit (alist)
	(cond ((null alist) 0)
		  ((null (first alist)) (summit (rest alist)))
		  (t (+ (first alist) (summit (rest alist))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
