;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: lisp11.lisp
;;;;    System: hw exercises
;;;;    Author: Taylor Olson
;;;;   Created: October 2, 2018 08:02:46
;;;;   Purpose: map-range, every-range, find-range, reduce-range
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun map-range (fn start end)
  (do ((increment (if (> start end) #'1- #'1+))
       (i start (funcall increment i))
       (lst '() (cons (funcall fn i) lst)))
      ((= i end) (reverse lst))))

(defun find-range (fn start end)
  (do ((increment (if (> start end) #'1- #'1+))
       (i start (funcall increment i)))
      ((= i end) nil)
    (when (funcall fn i) (return i))))
    
(defun every-range (fn start end)
  (do ((increment (if (> start end) #'1- #'1+))
       (i start (funcall increment i)))
      ((= i end) t)
    (unless (funcall fn i) (return nil))))

(defun reduce-range (fn start end &optional init)
  (do ((increment (if (> start end) #'1- #'1+))
       (i start (funcall increment i))
       (val init (funcall fn val i)))
      ((= i end) val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
