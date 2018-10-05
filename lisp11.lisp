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
  (if (> start end)
    (map-range-helper fn start end '-)
    (map-range-helper fn start end '+)))

(defun map-range-helper (fn start end sub-add)
  (if (= start end)
    '()
    (cons (funcall fn start) (map-range-helper fn (funcall sub-add start 1) end sub-add))))

(defun find-range (fn start end)
  (if (> start end)
    (find-range-helper fn start end '-)
    (find-range-helper fn start end '+)))

(defun find-range-helper (fn start end sub-add)
  (cond ((= start end) nil)
        ((funcall fn start) start)
        (t (find-range-helper fn (funcall sub-add start 1) end sub-add))))

(defun every-range (fn start end)
  (if (> start end)
    (every-range-helper fn start end '-)
    (every-range-helper fn start end '+)))

(defun every-range-helper (fn start end sub-add)
  (cond ((= start end) t)
        ((funcall fn start) (every-range-helper fn (funcall sub-add start 1) end sub-add)) 
        (t nil)))

(defun reduce-range (fn start end &optional init)
  (if (> start end)
    (reduce-range-helper fn start end '- init)
    (reduce-range-helper fn start end '+ init)))

(defun reduce-range-helper (fn start end sub-add &optional init)
  (if (= start end)
    init
    (reduce-range-helper fn (funcall sub-add start 1) end sub-add (funcall fn init start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
