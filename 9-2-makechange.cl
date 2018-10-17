;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 9-2-makechange
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 16, 2018 01:01:46
;;;;   Purpose: 


(in-package :cs325-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-change (cents &optional (coins '(25 10 5 1)))
  (do ((return-coins '() (cons (floor cur-cents (car cur-coins)) return-coins))
       (cur-cents cents (mod cur-cents (car cur-coins)))
       (cur-coins coins (rest cur-coins)))
      ((null cur-coins) (values-list (reverse return-coins)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
