;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: atomize
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: November 12, 2018 09:15:59
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------


(in-package :CS325-USER)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atomize (string)
  (let ((new-string (pre-process string))
        (result '())
        (stream (make-string-output-stream)))
    (loop for char across new-string
        do (if (or (char= #\newline char) (char= #\tab char)
                   (char= #\space char) (char= #\, char))
             (push (cur-token stream) result)
             (write-char char stream)))
    (push (cur-token stream) result)
    (reverse (remove nil result))))

(defun cur-token (stream)
  (symbolize (get-output-stream-string stream)))

(defun symbolize (string)
  (if (ignore-errors (numberp (read-from-string string)))
    (parse-integer string)
    (intern (string-upcase string))))

(defun pre-process (string)
  (remove #\" (handle-proper-nouns (string-trim '(#\space) string) 0)))

(defun handle-proper-nouns (string start &optional (return-string string))
  (let ((quot1 (position #\" string :start start)))
    (if quot1
      (let ((quot2 (position #\" string :start (1+ quot1))))
        (handle-proper-nouns string (1+ quot2)
                            (nsubstitute #\_ #\space string :start quot1 :end quot2)))
      return-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
