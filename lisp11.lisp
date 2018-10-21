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
  (do ((increment (if (> start end) -1 1))
       (i start (+ increment i))
       (lst '() (cons (funcall fn i) lst)))
      ((= i end) (reverse lst))))

(defun find-range (fn start end)
  (do ((increment (if (> start end) -1 1))
       (i start (+ increment i)))
      ((or (= i end) (funcall fn i)) (if (= i end) nil i))))
       
(defun every-range (fn start end)
  (do ((increment (if (> start end) -1 1))
       (i start (+ increment i)))
      ((or (= i end) (not (funcall fn i))) (= i end))))

(defun reduce-range (fn start end &optional init)
  (do ((increment (if (> start end) -1 1))
       (i start (+ increment i))
       (val init (funcall fn val i)))
      ((= i end) val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
