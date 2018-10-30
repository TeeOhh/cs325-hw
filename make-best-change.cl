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
  ;;(if null coins then '())
  ;;(get-min (apply +' (mapcar (lambda (x) (multiple-value-setq (quotient remainder) (floor remainder x)) quotient)
  ;;                coins)) (make-best-change cents (rest coins))
  (let ((remainder cents))
    (values-list (mapcar (lambda (x) (multiple-value-setq (quotient remainder) (floor remainder x)) quotient)
                   coins)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
