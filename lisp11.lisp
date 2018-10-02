;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: lisp11
;;;;    System: hw exercises
;;;;    Author: Taylor Olson
;;;;   Created: October 2, 2018 08:02:46
;;;;   Purpose: map-range, every-range, find-range, reduce-range
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: $
;;;;  $LastChangedBy: 
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun map-range (fn start end)
  (cond ((> start end) (cons (funcall fn start) (map-range fn (- start 1) end)))
        ((< start end) (cons (funcall fn start) (map-range fn (+ start 1) end)))
        (t ())
        ))

(defun find-range (fn start end)
   (cond ((= start end) nil)
         ((funcall fn start) start)
         ((> start end) (find-range fn (- start 1) end))
         (t (find-range fn (+ start 1) end)))
   )

(defun every-range (fn start end)
    (cond ((= start end) t)
          ((funcall fn start) 
            (if (> start end)
                (every-range fn (- start 1) end)
                (every-range fn (+ start 1) end)
            ))
          (t nil)
        )
    )

(defun reduce-range (fn start end &optional init)
    (cond ((= start end ) init)
          ((> start end) (reduce-range fn (- start 1) end (funcall fn init start)))
          (t (reduce-range fn (+ start 1) end (funcall fn init start)))
        )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
