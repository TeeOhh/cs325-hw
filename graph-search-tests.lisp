(in-package :cs325-user)

(defconstant *movie-data* "~/quicklisp/local-projects/cs325/movie-triples.txt")


(define-test query-search
  (ensure-data *movie-data*)
  (assert-equal 1850 (length (query-search '(?movie actor ?actor))))
  (assert-equal 895 (length (query-search '(?movie actress ?actress))))
  (assert-equal 2 (length (query-search '(?movie actor john_cho))))
  )

(define-test graph-search
  (ensure-data *movie-data*)
  (assert-equal 1850 (length (graph-search '((?movie actor ?actor)))))
  (assert-equal 895 (length (graph-search '((?movie actress ?actress)))))
  (assert-equal 
   2 
   (length (graph-search '((?movie actor john_cho) (?movie movie ?year)))))
  (assert-equal 
   2
   (length
    (graph-search '((?movie1 movie 1999) (?movie2 movie 2000) 
                    (?movie1 actress ?actress) (?movie2 actress ?actress)))))
  (assert-equal 
   1
   (length
    (graph-search '((?movie actor tom_waits) (?movie actor tom_cruise)))))
  (assert-equal
   9
   (length
    (graph-search '((?movie director ?person) (?movie actor ?person)))))
  )

(define-test graph-search-not
  (ensure-data *movie-data*)
  (assert-equal
   61
   (length
    (graph-search '((?movie movie ?year)
                    (:not (?movie actor tom_cruise))))))
  ;;; 24 movies with Scarlett Johansson...
  (assert-equal
   24
   (length (graph-search '((?movie actress SCARLETT_JOHANSSON)))))
  ;;;... but only 1 where the director wasn't an actor in some movie
  ;;; example of a graph in :not that introduces a new variable
  (assert-equal
   1
   (length
    (graph-search '((?movie actress SCARLETT_JOHANSSON) 
                    (:not (?movie director ?director) (?movie2 actor ?director))))))
  )

(define-test graph-search-filter
  (ensure-data *movie-data*)
  (assert-equal
   2
   (length
    (graph-search '((?movie actor tom_cruise) (?movie movie ?year)
                    (:filter (> ?year 1990))))))
  (assert-equal
   2
   (length (graph-search '((?movie1 movie 1999) (?movie2 movie 1999) 
                           (:filter (not-equal ?movie1 ?movie2))
                           (?movie1 actress ?actress) 
                           (?movie2 actress ?actress)))))
  )

(define-test filter-blists
  (assert-equal '(((?x 10))) 
                (query-search '(:filter (> ?x 1)) '(((?x 10)) ((?x 0)))))
  (assert-equal '(((?y b) (?x a))) 
                (query-search '(:filter (equal ?x a)) '(((?y b) (?x a)) ((?y a) (?x b)))))
  (assert-equal '() 
                (query-search '(:filter (equal ?x nil)) '(((?y b) (?x a)) ((?y a) (?x b)))))
  (assert-equal '() 
                (query-search '(:filter (equal ?x nil)) '(((?y b) (?x a)) ((?y a)))))
  (assert-equal '(((?y a) (?x nil))) 
                (query-search '(:filter (equal ?x nil)) '(((?y b) (?x a)) ((?y a) (?x nil)))))
  (assert-equal '(((?x 10))) 
                (query-search '(:filter (> ?x 1) (< ?x 100))
                               '(((?x 10)) ((?x 0)) ((?x 200)))))
  )

(define-test atomize
  ;; simple symbols and numbers
  (assert-equal '(abc 120) (atomize "  abc    120"))
  ;; a string in the string
  (assert-equal '(tom_cruise age 56) (atomize " \"Tom Cruise\" age 56"))
  ;; braces in the string
  (assert-equal '(filter |(| x > 10 |)|) (atomize "FILTER ( x > 10 )"))
  ;; dot in the string
  (assert-equal '(a b c \. d e f) (atomize "a b c . d e f"))
  ;; a full select form
  (assert-equal
   '(select ?movie ?year where { ?movie movie ?year filter not exists { ?movie actor tom_cruise } })
   (atomize "SELECT ?movie, ?year  WHERE {   ?movie movie ?year   FILTER NOT EXISTS {     ?movie actor \"Tom Cruise\"    } }"))
  ;; a select form with newlines in the string
  (assert-equal
   '(select ?movie ?year where { ?movie movie ?year \. ?movie actor tom_cruise filter |(| ?year > 1990 |)| })
   (atomize "SELECT ?movie, ?year 
WHERE {
  ?movie movie ?year .
  ?movie actor \"Tom Cruise\"
  FILTER (
    ?year > 1990
  )
}"))
  )

;;; Creates graph-search queries from SELECT form
;;; A separate simpler bit of code would get the SELECT variables
;;; to prune what's returned to a client
(define-test select-queries
  (assert-equal
   '((?movie movie ?year) (:not (?movie actor tom_cruise)))
   (select-queries
    "SELECT ?movie, ?year  WHERE {   ?movie movie ?year   FILTER NOT EXISTS {     ?movie actor \"Tom Cruise\"    } }"))
  (assert-equal
   '((?movie actor tom_cruise) (?movie movie ?year)
                    (:filter (> ?year 1990)))
   (select-queries
    "SELECT ?movie, ?year 
WHERE {
  ?movie movie ?year .
  ?movie actor \"Tom Cruise\"
  FILTER (
    ?year > 1990
  )
}"))
  (assert-equal
   '((?movie1 movie 1999) (?movie2 movie 1999) 
                           (:filter (not-equal ?movie1 ?movie2))
                           (?movie1 actress ?actress) 
                           (?movie2 actress ?actress))
   (select-queries
    "SELECT ?movie1, ?movie2, ?actress 
WHERE {
  ?movie1 movie 1999 .
  ?movie2 movie 1999 .
  ?movie1 actress ?actress .
  ?movie2 actress ?actress
  FILTER (
    ?movie1 != ?movie2
  )
}"))
  )
