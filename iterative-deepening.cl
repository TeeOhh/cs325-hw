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
                (let ((x end)) #'(lambda (y) (eql x y)))
                (let ((paths net)) #'(lambda (path) (cdr (assoc (car path) paths))))
                end)))

(defun ids (path pred gen)
  ;while (dls path pred gen n) = nil
  ;(dls path pred gen (1+ n))
  ;if extend failure, return no path found
  (do ((n 0 (1+ n))
       (result nil (dls path pred gen n)))
      ((or (eql result 'extend-failure)
           (member end result)) 
       (if (eql result 'extend-failure) nil result))))

(defun dls (path pred gen n depth)
  (cond ((null path) nil) ;found a cycle
        ((funcall pred (car path)) path)
        ((= depth n) path) ;depth failure, need to indicate that it didn't find end
        (t (let ((neighbors (funcall gen path)))
             (if (null neighbors)
               ;extend failure
               ;need to collect these
               (loop for neighbor in neighbors
                   do (dls (build-path neighbor path) pred gen (1+ depth))))))))

(defun build-path (neighbor path)
  (if (member neighbor path)
    nil
    (cons neighbor path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
