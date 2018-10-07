;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 2-4789
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 2, 2018 11:21:21
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun greater (a b)
  (cond ((> a b) a)
        (t b)))

(defun has-list-p (alist)
  (cond ((null alist) nil)
        ((listp (car alist)) t)
        (t (has-list-p (rest alist)))))

(defun print-dots (num)
  (do ((i 1 (1+ i)))
      ((> i num) t)
    (princ ".")))

(defun print-dots (num)
  (when (> num 0)
    (princ ".")
    (print-dots (1- num))))

(defun get-a-count (alist)
  (do ((i 0 (1+ i))
       (cur alist (rest cur))
       (sum 0 (if (eql (first cur) 'a) (1+ sum) sum)))
      ((= i (length alist)) sum)))

(defun get-a-count (alist)
  (cond ((null alist) 0)
        ((eql (first alist) 'a) (1+ (get-a-count (rest alist))))
        (t (get-a-count (rest alist)))))

;; The "remove nil lst" portion will return a version of the list with
;; nil removed, it will not save over the passed in lst. If instead
;; he were to nest the remove inside the apply, it would work.

(defun summit (alist)
  (apply #'+ (mapcar (lambda (x) (if x x 0)) alist)))

;; He never checks to see when the lst is empty (no base case),
;; so the recursion will go on forever. Taking the cdr
;; of '() and then "nil" forever on.

(defun summit (alist)
  (cond ((null alist) 0)
        ((null (first alist)) (summit (rest alist)))
        (t (+ (first alist) (summit (rest alist))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
