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
  ;;find start next token (based on end of cur token)
  ;;find end of next token (based on start of it)
  ;;if token found, set tokenizer-cur to this token. Else set as nil
  (get-start-end tok)
  )


(defun make-tokenizer (string &optional (delimiter #\space))
  (setf obj (make-instance 'tokenizer :string string :delimiter delimiter :cur (list 0 (length string))))
  
  ;;call next-token once to initialize pointer
  )

(defun get-start-end ((tok tokenizer))
  (if (string= (tokenizer-delimeter tok) #\space)
    (position-if-not (lambda (x) (string= x (tokenizer-delimeter tok))) (tokenizer-string tok))
    ;;else, can't return nil of nothing but delimeters found
    'test
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
