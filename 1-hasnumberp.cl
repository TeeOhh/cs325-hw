;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 1-hasnumberp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 24, 2018 15:01:07
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package ::cs325-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun has-number-p (expr)
  (cond ((atom expr) (numberp expr))
        (t (or (has-number-p-test (first expr)) (has-number-p (rest expr))))))

(defun has-number-p-test (expr)
  (if (listp expr)
    (some #'numberp expr)
    (numberp expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
