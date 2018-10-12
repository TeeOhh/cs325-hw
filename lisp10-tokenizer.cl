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
              :initarg :delimeter)
   (cur :accessor tokenizer-cur :initarg :cur)))

(defmethod next-token-p ((tok tokenizer))
  (> (length (tokenizer-cur tok)) 0))

(defmethod next-token ((tok tokenizer))
  (if (string= (tokenizer-delimeter tok) #\space)
    (loop while (and (next-token-p tok) (string= (subseq (tokenizer-cur tok) 0 1) (tokenizer-delimeter tok)))
        do (setf (tokenizer-cur tok) (subseq (tokenizer-cur tok) 1))))
  (if (next-token-p tok)
    (let ((return-token nil))
      (setf return-token (subseq (tokenizer-cur tok) 0 (position (tokenizer-delimeter tok) (tokenizer-cur tok))))
      (if (null return-token)
        (progn (setf return-token (tokenizer-cur tok)) (setf (tokenizer-cur tok) ""))
        (progn (setf (tokenizer-cur tok) (subseq (tokenizer-cur tok) (position (tokenizer-delimeter tok) (tokenizer-cur tok))))
          (loop while (and (next-token-p tok) (string= (subseq (tokenizer-cur tok) 0 1) (tokenizer-delimeter tok)))
                do (setf (tokenizer-cur tok) (subseq (tokenizer-cur tok) 1)))))
      return-token)))

(defun make-tokenizer (string &optional (delimeter #\space))
  (setf obj (make-instance 'tokenizer :string string :delimeter delimeter :cur string)))

(defun split-string (str &optional (delim #\space))
  (let ((tr (make-tokenizer str delim)))
    (do ((l nil (cons (next-token tr) l)))
      ((not (next-token-p tr)) (nreverse l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
