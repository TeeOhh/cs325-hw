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
      (let* ((node (car path)) (paths-to-add (new-paths path node net end)))
        ;;do mapcan over (cdr (assoc node net))
        ;;cond (eql n end) (reverse (cons n path)) this is exit condition for recursion 
        (if (member end paths-to-add)
          paths-to-add
          (bfs end (append (cdr queue) paths-to-add) net))))))

(defun new-paths (path node net end)
  (mapcan #'(lambda (n)
              (cond ((eql n end) (reverse (cons n path)))
                    ((not (member n path)) (list (cons n path)))
                    (t nil)))
    (cdr (assoc node net))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
