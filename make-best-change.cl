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

(defun get-best (cur best)
  (let ((cur-temp (apply '+ cur)) (best-temp (apply '+ best)))
    (if (< cur-temp best-temp) cur best)))

(defun make-best-change-denom (cents &optional (coins '(25 10 5 1)))
  ;setf test (mapcar (lambda (x) (multiple-value-setq (quotient remainder) (floor remainder x)) quotient)
  ;                   coins)
  ;(cons (1- (car test)) (make-best-change-denom ((- cents (

;;;(defun get-best (cur best)
;;;  (let ((cur-temp (apply '+ cur)) (best-temp (apply '+ (car best))))
;;;    (cond ((< remainder (cdr best)) (cons cur remainder))
;;;          ((= remainder (cdr best)) (cond ((< cur-temp best-temp) cur)
;;;                                          (t best)))
;;;          (t best))))

(defun sparse-to-dense (best coins)
  (do ((zeros-needed  (- (length coins) (length best)))
       (amount 0 (1+ amount))
       (return-change best (cons 0 return-change)))
      ((eql amount zeros-needed) return-change)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
