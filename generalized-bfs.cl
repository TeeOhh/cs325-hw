;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: generalized-bfs
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: November 17, 2018 16:30:00
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :CS325-USER)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shortest-path (start end net)
  (reverse (bfs (list (list start))
                (let ((x end)) #'(lambda (y) (eql x y)))
                (let ((paths net)) #'(lambda (path) (cdr (assoc (car path) paths))))
                end)))

(defun bfs (queue pred gen end)
  (if (empty-queue-p queue)
    nil
    (let* ((path (car queue)) (neighbors (funcall gen path)))
      (if (some pred neighbors) (cons end path)
        (bfs (append (cdr queue) (cycle-checker path neighbors)) pred gen end)))))

(defun cycle-checker (path neighbors)
  (mapcan #'(lambda (n)
              (cond ((member n path) nil)
                    (t (list (cons n path)))))
    neighbors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
