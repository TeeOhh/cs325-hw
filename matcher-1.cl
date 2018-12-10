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
  (and x (append (match-p (car x) y lsts)
                 (?or (cdr x) y lsts))))

(defun ?= (pattern form extra)
  (let ((fn (cadr pattern))
        (args (cddr pattern)))
    (match-p (car pattern) (apply fn form args) extra)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
