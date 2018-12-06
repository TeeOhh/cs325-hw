;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: has-slot-p
;;;;    System: 
;;;;    Author: 
;;;;   Created: December 4, 2018 19:29:19
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package #:mop-tests)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun has-slots-p (mop slots)
  (every (lambda (slot) (has-slots-helper (all-absts mop) slot)) slots))

(defun has-slots-helper (mops slot)
  (if (null mops)
    nil
    (let* ((cur-mop (get-mop (car mops))) (filler (find-filler cur-mop slot)))
      (unless filler (has-slots-helper (cdr mops) slot))
      ;need to check here if (cdr filler) inherets slot
      (when (equal (cdr slot) (cdr filler)) t))))

(defun find-filler (mop slot)
  (assoc (car slot) (cddr mop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
