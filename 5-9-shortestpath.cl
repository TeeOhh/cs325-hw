;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: 5-9-shortestpath
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 16, 2018 15:12:59
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(defun shortest-path (start end net)
;;;  (catch 'found (bfs end (list (list start)) net)))
;;;
;;;(defun bfs (end queue net)
;;;  (if (empty-queue-p queue)
;;;    nil
;;;    (let ((path (car queue)))
;;;      (let ((node (car path)))
;;;        (bfs end (append (cdr queue) (new-paths path node net end)) net)))))
;;;
;;;(defun new-paths (path node net end)
;;;  (mapcan #'(lambda (n)
;;;              (cond ((eql n end) (throw 'found (reverse (cons n path))))
;;;                    ((not (member n path)) (list (cons n path)))
;;;                    (t nil)))
;;;    (cdr (assoc node net))))


(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
    nil
    (let ((path (car queue)))
      (let* ((node (car path)) (neighbors (cdr (assoc node net))))
        (if (member end neighbors)
          (reverse (cons end path))
          (bfs end (append (cdr queue) (new-paths path end neighbors)) net))))))

(defun dfs (start end net)
  ;;for neighbor of start
  ;;if no neighbors return nil
  ;;if neighbor = end
  ;;  if (length cur-path) > (length longest-path)
  ;;     cur-path
  ;;     longest-path
  ;;else, recall with neighbor = start, cur-path, longest-path
  )

(defun new-paths (path end neighbors)
  (mapcan #'(lambda (n)
              (cond ((not (member n path)) (list (cons n path)))
                    (t nil)))
    neighbors))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
