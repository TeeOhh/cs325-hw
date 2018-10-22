;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 10-35Macros
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 22, 2018 08:24:21
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro nth-expr (n &rest args)
  (let ((arg `(nth (1- ,n) (list ,@args))))
    arg))
  ;;`(nth (1- ,n) (list ,@args)))

(defmacro n-of (n expr)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
