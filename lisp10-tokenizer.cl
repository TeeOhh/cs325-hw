;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: lisp10-tokenizer
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: October 9, 2018 09:53:02
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------

(in-package :cs325-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tokenizer ()
  ((string :accessor tokenizer-string :initarg :string)
   (delimeter :accessor tokenizer-delimeter
              :initarg :delimiter)
   (cur :accessor tokenizer-cur :initarg :cur)))

(defmethod next-token-p ((tok tokenizer))
  (not (null (tokenizer-cur tok))))

(defmethod next-token ((tok tokenizer))
  )


(defun make-tokenizer (string &optional (delimiter #\space))
  (setf obj (make-instance 'tokenizer :string string :delimiter delimiter :cur string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
