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
  ;;initally call dfs with: start end net start '()
  (dfs start end net (list start) '()))

(defun dfs (start end net path longest-tracker)
  ;;for neighbor of start
  ;;if no neighbors return nil
  ;;if neighbor = end
  ;;  if (length cur-path) > (length longest-tracker)
  ;;     cur-path
  ;;     longest-tracker
  ;;else, recall with neighbor = start, cur-path, longest-path
  (let ((neighbors (cdr (assoc start net))))
    (if (null neighbors) longest-tracker
      (loop for neighbor in neighbors
            do (cond ((string= neighbor end) 
                   (if (> (length (cons neighbor path)) (length longest-tracker)) (cons neighbor path) longest-tracker))
                  ((member neighbor path) nil)
                  (t (dfs neighbor end net (cons neighbor path) longest-tracker)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
