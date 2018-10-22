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
   (delimeter :accessor tokenizer-delimiter
              :initarg :delimiter)
   (cur :accessor tokenizer-cur :initarg :cur)))

(defmethod next-token-p ((tok tokenizer))
  (not (null (tokenizer-cur tok))))

(defmethod next-token ((tok tokenizer))
  (let ((cur (tokenizer-cur tok)) (delim (tokenizer-delimiter tok)) (string (tokenizer-string tok)))
    (setf (tokenizer-cur tok) (get-start-end cur delim string))
    (subseq string (first cur) (second cur))))

(defun make-tokenizer (string &optional (delimiter #\space))
  (setf obj (make-instance 'tokenizer :string string :delimiter delimiter 
              :cur (get-start-end (list -1 -1) delimiter string))))

(defun get-start-end (cur delim string)
  (let ((start (get-start delim string cur)))
    (if (or (not start) (eql start (1+ (length string))))
      nil
      (list start (get-end start delim string)))))

(defun get-start (delimiter string current)
  (if (string= delimiter #\space)
    (position-if-not (lambda (x) (string= x delimiter)) string :start (1+ (second current)))
    (1+ (second current))))

(defun get-end (start delimiter string)
  (let ((end (position delimiter string :start start)))
    (if end end (length string))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
