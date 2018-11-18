(in-package :cs325-user)

(defvar *triples* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File loading 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-data (pathname)
  (when (null *triples*)
    (load-triples pathname)))

(defun clear-triples ()
  (setq *triples* nil))

(defun load-triples (pathname)
  (with-open-file (in pathname)
    (do ((triple (read in nil in) (read in nil in))
         (n 0 (1+ n)))
        ((eq triple in) n)
      (add-triple triple))))

(defun add-triple (triple)
  (unless (member triple *triples* :test 'equal)
    (push triple *triples*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query searching and matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun graph-search (queries &optional (blists (list nil)))
  (do ((blists blists (query-search (car queries) blists))
       (queries queries (cdr queries)))
      ((or (null blists) (null queries)) 
       blists)))

(defun query-search (query &optional (blists (list nil)))
  (cond ((null blists) nil)
        ((member :not query)
         (get-non-match blists (graph-search (cdr query) blists)))
        (t (mapcan (lambda (triple)
                     (match-query query triple blists))
             *triples*))))

(defun get-non-match (blists not-bindings)    
  (loop for binding in blists
      when (null (loop for binding2 in not-bindings
                  when (match-query (car binding) (car (last binding2)))
                     collect binding))
        collect binding))
                               

(defun match-query (query triple &optional (blists (list nil)))
  (cond ((null blists) nil)
        ((null query) blists)
        (t (match-query (cdr query) (cdr triple)
                        (match-item (car query) (car triple) blists)))))

(defun match-item (pat obj blists)
  (mapcan (lambda (blist)
            (cond ((eql pat obj) (list blist))
                  ((var-p pat) (match-var pat obj blist))
                  (t nil)))
          blists))

(defun match-var (var val blist)
  (let ((binding (assoc var blist)))
    (cond ((null binding) (list (cons (list var val) blist)))
          ((eql val (cadr binding)) (list blist))
          (t nil))))

(defun var-p (pat)
  (and (symbolp pat) (char= (char (symbol-name pat) 0) #\?)))
