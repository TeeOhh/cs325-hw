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
(defun map-stream (fn in)
  (loop for expr = (read in nil in)
      until (eql exp stream)
        do (funcall fn expr)))

(defun map-file (fn pathname)
  (with-open-file (in pathname)
    (map-stream fn in)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
