;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: matcher-1
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 8, 2018 09:15:17
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)
(in-package #:exmatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ?not (x y lsts)
  (cond ((null lsts) lsts)
        ((null x) lsts)
        (t (?not (cdr x) y (match-p (car x) y lsts)))))

(defun ?or (patterns y lsts)
  (match-p patterns y lsts))

(defun ?= (pattern))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
