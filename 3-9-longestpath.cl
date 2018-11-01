;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 3-9-longestpath
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 23, 2018 09:41:57
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------


(in-package :cs325-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun longest-path (start end net)
  (let ((initial-longest (if (string= start end) (list start) '())))
    (reverse (dfs end net (list start) (cdr (assoc start net)) initial-longest))))

(defun dfs (end net path neighbors longest-tracker)
  (if (null neighbors) longest-tracker
    (dfs end net path (rest neighbors)
         (get-longest (car neighbors) end path net longest-tracker))))

(defun get-longest (node end path net longest-tracker)
  (let ((new-path (cons node path)))
    (cond ((string= node end) (get-longer new-path longest-tracker))
          ((member node path) longest-tracker)
          (t (dfs end net new-path (cdr (assoc node net)) longest-tracker)))))

(defun get-longer (list1 list2)
  (if (> (length list1) (length list2)) list1 list2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
