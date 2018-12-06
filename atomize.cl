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

;I refactored into subfunctions but not sure if rebuilding logic to "next-token" function
;is time well spent. I'd like to spend the last few days on other AI exercises.

(defun atomize (string)
  (parse-string (pre-process string)))

(defun parse-string (string)
  (let ((result '())
        (stream (make-string-output-stream)))
    (loop for char across string
        do (if (space-equiv-p char)
             (setf result (push-nonempty-token (get-output-stream-string stream) result))
             (write-char char stream)))
    (setf result (push-nonempty-token (get-output-stream-string stream) result))
    (reverse result)))

(defun push-nonempty-token (cur-string result)
  (if (string= "" cur-string)
    result
    (cons (symbolize cur-string) result)))

(defun space-equiv-p (char)
  (or (char= #\newline char)
      (char= #\tab char)
      (char= #\space char)
      (char= #\, char)))

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
