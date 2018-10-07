;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 5-8
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 6, 2018 13:21:39
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max-min (vec &key (start 0) (end (length vec)))
  (if (= start end)
    (values nil nil)
    (let ((init (svref vec start)))
      (max-min-helper vec :start (1+ start) :end end :max init :min init))))

(defun max-min-helper (vec &key start end max min)
  (if (= start end)
    (values max min)
    (let ((cur (svref vec start)))
      (max-min-helper vec :start (1+ start) :end end 
                :max (if (> cur max) cur max) :min (if (< cur min) cur min)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
