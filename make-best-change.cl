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
  (if (member 1 coins)
    (pennies cents coins)
    (no-pennies cents coins)))

(defun no-pennies (cents coins)
  (multiple-value-bind (quotient remainder) (floor cents (car coins))
    (cond ((null (cdr coins)) remainder)
          (t (get-best (no-pennies remainder (rest coins))
                       (no-pennies-helper remainder coins quotient))))))

(defun no-pennies-helper (remainder coins past-quotient)
  (cond ((eql past-quotient 0) remainder)
        (t (get-best (no-pennies-helper (+ remainder (car coins)) coins (1- past-quotient))
                      (no-pennies (+ remainder (car coins)) (rest coins))))))

(defun get-best (val1 val2)
  (if (< remainder1 remainder2) remainder1 remainder2))

(defun pennies (cents coins)
  (do* ((remainder cents cents)
        (current '() (mapcar (lambda (x) (multiple-value-setq (quotient remainder) (floor remainder x)) quotient)
                      new-coins))
        (best-change (list (1+ cents)) (get-best-pennies current best-change))
        (new-coins coins (rest new-coins)))
       ((null new-coins) (values-list (sparse-to-dense best-change coins)))))

(defun get-best-pennies (cur best)
  (let ((cur-temp (apply '+ cur)) (best-temp (apply '+ best)))
    (if (< cur-temp best-temp) cur best))) 

(defun sparse-to-dense (best coins)
  (do ((zeros-needed  (- (length coins) (length best)))
       (amount 0 (1+ amount))
       (return-change best (cons 0 return-change)))
      ((eql amount zeros-needed) return-change)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
