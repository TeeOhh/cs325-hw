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

;;if last denom is a 1, then can do this, other wise have to check all possibilities
(defun make-best-change (cents &optional (coins '(25 10 5 1)))
  (do* ((remainder cents cents)
        (current '() (mapcar (lambda (x) (multiple-value-setq (quotient remainder) (floor remainder x)) quotient)
                      new-coins))
        (best-change (list (1+ cents)) (get-best current best-change))
        (new-coins coins (rest new-coins)))
       ((null new-coins) (values-list (sparse-to-dense best-change coins)))))

;at first call no-pennis with (cents - quotient * (car coins)) '() (rest coins) quotient)

(defun no-pennies (cents past-coins coins shortest-tracker)
  ;if quotient = 0 and (null cons) shortest-tracker
  ;if quotient = 0 re-call with past-coins (rest coins)
  ;(multiple-value-setq (quotient remainder) (floor cents (car coins))
  ;store quotient
  ;new = (mapcar (lambda (x) (multiple-value-setq (quotient remainder) (floor remainder x)) quotient)
  ;(rest coins))
  ;recall with past-coins = quotient - 1,(get-best (cons past-coins new) shortest-tracker)
  )


(defun get-best (cur best)
  (let ((cur-temp (apply '+ cur)) (best-temp (apply '+ best)))
    (if (< cur-temp best-temp) cur best)))

(defun sparse-to-dense (best coins)
  (do ((zeros-needed  (- (length coins) (length best)))
       (amount 0 (1+ amount))
       (return-change best (cons 0 return-change)))
      ((eql amount zeros-needed) return-change)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
