;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 9-2-makechange
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 16, 2018 01:01:46
;;;;   Purpose: 


(in-package :cs325-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-change (cents &optional (coins '(25 10 5 1)))
  (let ((remainder cents))
    (values-list (mapcar (lambda (x) (multiple-value-setq (quotient remainder) (floor remainder x)) quotient)
    coins))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
