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
  (let ((longest (reverse (dfs start end net (list start) (cdr (assoc start net)) '()))))
    (if (and (null longest) (eql start end))
      (list start)
      longest)))

(defun dfs (start end net path neighbors longest-tracker)
  (if (null neighbors) longest-tracker
    (dfs start end net path (rest neighbors)
         (get-longest (car neighbors) end path net longest-tracker))))

(defun get-longest (node end path net longest-tracker)
  (let ((new-path (cons node path)))
    (cond ((string= node end) (if (> (length new-path) (length longest-tracker))
                                new-path longest-tracker))
          ((member node path) longest-tracker)
          (t (dfs node end net new-path (cdr (assoc node net)) longest-tracker)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
