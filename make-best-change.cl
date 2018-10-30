;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: make-best-change
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 29, 2018 21:47:36
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------


(in-package :CS325-USER)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-best-change (cents &optional (coins '(25 10 5 1)))
  (do* ((new-coins coins (rest coins))
       (remainder cents cents)
       (current '() (mapcar (lambda (x) (multiple-value-setq (quotient remainder) (floor remainder x)) quotient)
                   new-coins))
        (best-change '() current))
       ;;add (length of coins - new-coins) zeros to front of best-change 
      ((null new-coins) best-change)))
    
(defun get-best (cur best)
  (if (< (apply +' cur) (apply +' best)) cur best))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
