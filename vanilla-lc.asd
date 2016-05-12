
(asdf:defsystem vanilla-lc
  :name "Vanilla Lambda Calculus"
  :version "0.1.0"
  :author "Carl Olson"
  :licence "Public Domain"
  :description "A simple, functional, and fast lambda calculus interpreter."
  :depends-on (:alexandria :iterate :trivia :yacc)
  :serial t
  :components ((:file "package")
	       (:file "helpers")
	       (:file "parser")
	       (:file "de-bruijn")
	       (:file "main")))
