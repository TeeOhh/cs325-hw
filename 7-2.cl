;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 7-2
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 8, 2018 09:32:48
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun map-stream (fn stream)
  (loop for expr = (read stream nil stream)
      until (streamp expr)
        do (funcall fn expr)))

(defun map-file (fn pathname)
  (with-open-file (stream pathname)
    (loop for expr = (read stream nil stream)
      until (streamp expr)
        do (funcall fn expr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
