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
  (unless (match-p (car x) y lsts) lsts))

(defun ?or (x y lsts)
  (mapcan (lambda (x-var) (match-p x-var y lsts)) x))

(defun ?= (sub-pattern form extra)
  (let* ((pattern (car sub-pattern)) (fn (cadr sub-pattern)) (args (cddr sub-pattern)))
    (mapcan #'(lambda (arg) (match-p pattern (funcall fn form arg) extra)) args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
