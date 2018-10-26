;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 3-8-show
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 24, 2018 23:23:30
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------


(in-package :cs325-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun show-dots (alist)
  (cond ((atom alist) (format t "~s" alist))
        (t 
         (format t "(")
         (show-dots (car alist))
         (format t " . ")
         (show-dots (cdr alist))
         (format t ")"))))

(defun show-list (alist)
  (cond ((atom alist) (format t "~s" alist))
        (t (format t "[") (show-list (car alist))
           (show-list-helper (cdr alist)) (format t "]"))))

(defun show-list-helper (alist)
  (cond ((null alist) nil)
        ((atom alist) (format t " . ~s" alist))
        (t (format t " ") (show-list (car alist)) (show-list-helper (cdr alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
