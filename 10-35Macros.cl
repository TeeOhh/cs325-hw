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
  ;;only want comma before (evaluated) nth arg of args
  ;;(nth ,n (list ,@args))))
  (let ((g (gensym)) (h (gensym)))
    `(let ((,g ,n) (,h ,args))
       (nth ,g ,@args))))

(defmacro n-of (n expr)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
