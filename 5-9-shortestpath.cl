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
(defun shortest-path (start end net)
  (catch 'found (bfs end (list (list start)) net)))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
          (reverse path)
          (bfs end
                
               (append (cdr queue)
                       (new-paths path node net end))
               net))))))

(defun new-paths (path node net end)
  (let ((new-path (mapcar #'(lambda (n)
              (cond ((eql n end) (throw 'found (reverse (cons n path))))
                    ((not (find n path)) (cons n path))
                    (t (values))))
                    (cdr (assoc node net)))))
    new-path))






(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (empty-queue-p queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
          (reverse path)
          (bfs end
               (let ((new-path (new-paths path node net end)))
                 (loop for path-cur in new-path
                       do (when (find end path-cur) (return-from bfs path-cur)))
                   (append (cdr queue)
                       new-path))
               net))))))


(defun new-paths (path node net end)
  (let ((new-path (mapcar #'(lambda (n)
              (cond ((eql n end) (reverse (cons n path)))
                    ((not (find n path)) (cons n path))
                    (t (values))))
                    (cdr (assoc node net)))))
    new-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
