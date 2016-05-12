
(defpackage :vanilla-lc
  (:use :cl :iterate)
  (:import-from :alexandria :compose)
  (:import-from :trivia     :defpattern :ematch :guard :guard1)
  (:import-from :yacc       :define-parser :parse-with-lexer)
  (:export :eval-string :eval-file :reduce-star
	   :mklambda1 :mkapp :lambda1 :app
	   :t1 :t2 :dirty :depth))
