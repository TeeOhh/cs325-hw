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
;; "no-pennies" does the iterating through all combinations and checks it against the best seen
;; using update-best when it sees the last coin.

;; The last 3 functions are used to separate the logic if there's a penny in coins.
;; (I still think there's a proof for this, haven't had the time to work through it yet)


(defun make-best-change (cents &optional (coins '(25 10 5 1)))
  (cond ((member 1 coins) (pennies cents coins))
        (t (let ((*current* (make-array (1+ (length coins)) :initial-element 0 :fill-pointer 0))
                 (*best* (make-array (1+ (length coins)) :initial-element 0)))
             (no-pennies cents coins)
             (values-list (subseq (coerce *best* 'list) 0 (length coins)))))))

(defun no-pennies (cents coins &optional)
  (multiple-value-bind (quotient remainder) (floor cents (car coins))
    (cond ((null (cdr coins))
           (vector-push quotient *current*)
           (vector-push remainder *current*)
           (update-best remainder))
          (t
           (do* ((quotient-tracker quotient (1- quotient-tracker))
                 (cents-tracker (- cents (* quotient-tracker (car coins)))
                                (- cents (* quotient-tracker (car coins))))
                 (fill-pointer (fill-pointer *current*)))
                ((= quotient-tracker -1))
             (vector-push quotient-tracker *current*)
             (no-pennies cents-tracker (rest coins))
             (setf (fill-pointer *current*) fill-pointer))))))

(defun update-best (remainder)
  (when (= (reduce #'+ *best*) 0) (replace *best* *current*))
  (when (< remainder (aref *best* (1- (length *best*))))
         (replace *best* *current*)))

(defun recalculate-cents (cents quotient coin)
  (- cents (* quotient coin)))





(defun pennies (cents coins)
  (do* ((remainder cents cents)
        (current '() (mapcar (lambda (x)
                               (multiple-value-setq (quotient remainder) (floor remainder x)) quotient)
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
