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
  (if (string= (tokenizer-delimeter tok) #\space)
    (progn
      (loop while (and (not (string= (tokenizer-cur tok) "")) (string= (subseq (tokenizer-cur tok) 0 1) #\space))
             do (setf (tokenizer-cur tok) (subseq (tokenizer-cur tok) 1)))
      (not (string= (tokenizer-cur tok) "")))
    (not (null (tokenizer-cur tok)))))

(defmethod next-token ((tok tokenizer))
  (when (string= (tokenizer-delimeter tok) #\space)
    (loop while (and (next-token-p tok) (string= (subseq (tokenizer-cur tok) 0 1) #\space))
        do (setf (tokenizer-cur tok) (subseq (tokenizer-cur tok) 1))))
  (unless (next-token-p tok) (return-from next-token nil))
  (let ((return-string ""))
    (loop while (and (not (string= (tokenizer-cur tok) "")) (not (string= (subseq (tokenizer-cur tok) 0 1) (tokenizer-delimeter tok))))
          do (setf return-string (concatenate 'string return-string (subseq (tokenizer-cur tok) 0 1)))
          (setf (tokenizer-cur tok) (subseq (tokenizer-cur tok) 1)))
    (cond ((not (next-token-p tok)) (return-from next-token return-string))
          ((string= (tokenizer-delimeter tok) #\space) 
           (loop while (and (next-token-p tok) (string= (subseq (tokenizer-cur tok) 0 1) #\space))
                  do (setf (tokenizer-cur tok) (subseq (tokenizer-cur tok) 1))))
          (t (setf (tokenizer-cur tok) (if (string= (tokenizer-cur tok) "") nil (subseq (tokenizer-cur tok) 1)))))
    return-string))


(defun make-tokenizer (string &optional (delimeter #\space))
  (setf obj (make-instance 'tokenizer :string string :delimeter delimeter :cur string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
