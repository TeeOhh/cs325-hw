;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: alldifferent-ddr
;;;;    System: 
;;;;    Author: 
;;;;   Created: November 18, 2018 17:01:01
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2018-09-27 12:08:59 -0500 (Thu, 27 Sep 2018) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package #:ddr-tests)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *all-different-kb*
  '((-> (different ?x ?y) (different ?y ?x))
    (-> (ad (cons ?x (cons ?y nil))) (different ?x ?y))
    (-> (ad (cons ?x (cons ?y ?l))) (different ?x ?y)
        (ad ?x ?l)
        (ad ?y ?l))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
