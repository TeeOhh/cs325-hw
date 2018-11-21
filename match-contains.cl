;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: match-contains
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: November 20, 2018 01:32:45
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------
(in-package #:exmatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ?contains (x y lsts)
  (cond ((null x) lsts)
        ((null y) (match-p (car x) y lsts))
        ((atom y) (match-p (car x) y lsts))
        (t (append (?contains (cdr x) y (match-p (car x) y lsts))
                   (?contains x (car y) lsts)
                   (?contains x (cdr y) lsts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
