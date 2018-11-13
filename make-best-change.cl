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
  (cond ((member 1 coins) (pennies cents coins))
        (t (defparameter *current* (make-hash-table))
           (defparameter *best* (make-hash-table))
           (no-pennies cents coins)
           (values-list (mapcar #'(lambda (coin) (gethash coin *best*)) coins)))))

(defun no-pennies (cents coins &optional)
  (multiple-value-bind (quotient remainder) (floor cents (car coins))
    (cond ((null (cdr coins)) 
           (update-tables quotient remainder coins))
          (t
           (do* ((quotient-tracker quotient (1- quotient-tracker))
                (cents-tracker (- cents (* quotient-tracker (car coins))) (- cents (* quotient-tracker (car coins)))))
               ((= quotient-tracker -1))
             (setf (gethash (car coins) *current*) quotient-tracker)
             (no-pennies cents-tracker (rest coins)))))))

(defun update-tables (quotient remainder coins)
  (setf (gethash (car coins) *current*) quotient)
  (setf (gethash 'remainder *current*) remainder)
  (when (= (hash-table-count *best*) 0)
    (maphash #'(lambda(key val) (setf (gethash key *best*) val)) *current*))
  (when (< remainder (gethash 'remainder *best*))
    (maphash #'(lambda(key val) (setf (gethash key *best*) val)) *current*)))

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
