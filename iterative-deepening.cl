;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: iterative-deepening
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: November 26, 2018 20:29:09
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :CS325-USER)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shortest-path (start end net)
  (reverse (ids (list (list start))
                (lambda (x) (eql x end))
                (lambda (path) (cdr (assoc (car path) net))))))

(defun ids (paths pred gen)
  (do* ((n 0 (1+ n))
        (result (dls (car paths) pred gen n 0) (dls (car paths) pred gen n 0)))
       ((cheap-test result) result)))

(defun dls (path pred gen n depth)
  (cond ((null path) nil)
        ((funcall pred (car path)) path)
        ((= depth n) 'depth-fail)
        (t (let ((neighbors (funcall gen path)))
             (if (null neighbors)
               nil
               (dls-helper neighbors path pred gen n depth))))))

(defun dls-helper (neighbors path pred gen n depth)
  (let ((result-path (dls (build-path (car neighbors) path) pred gen n (1+ depth))))
    (cond ((null (cdr neighbors)) result-path)
          (t result-path
             (dls-helper (cdr neighbors) path pred gen n depth)))))

(defun cheap-test (result)
  (or (null result)
      (not (eql 'depth-fail result))))

(defun build-path (neighbor path)
  (if (member neighbor path)
    nil
    (cons neighbor path)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
