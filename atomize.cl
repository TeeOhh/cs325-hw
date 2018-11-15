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
;make every character a symbol
;(intern string)

(defun atomize (string)
  (let ((new-string (remove #\" (strings-to-symbols (string-trim '(#\space) string)))))
    (with-output-to-string (s)
      (loop for char across new-string
          do (write-string (character-transform char) s)))))

;if eql newline, tab, or space
;   if string, (cons string list) then reset string buffer
;else (write-string (intern (string (char-downcase char))))

(defun character-transform (char)
  (cond ((or (eql #\newline char) (eql #\tab char)) (string #\space)) ;if string not nil, cons
        (t (string (char-downcase char)))))

(defun strings-to-symbols(string &optional (start 0) (return-string string))
  (let ((quot1 (position #\" string :start start)))
    (if quot1
      (let ((quot2 (position #\" string :start (1+ quot1))))
        (strings-to-symbols string (1+ quot2)
                            (substitute #\_ #\space string :start quot1 :end quot2)))
      return-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
