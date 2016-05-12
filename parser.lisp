(in-package :vanilla-lc)

(defun make-lexer (str)
  (let ((chars (string-chars str)))
    (labels ((self ()
	       (if (null chars)
		   (values nil nil)
		   (ematch (pop chars)
		     (#\-
		      ;; remove comments
		      (loop until (char= (pop chars) #\Newline))
		      (self))
		     (#\(
		      (values 'lparen #\())
		     (#\)
		      (values 'rparen #\)))
		     ((or #\\ #\λ)
		      (values 'lambda #\λ))
		     (#\.
		      (values 'period #\.))
		     (#\=
		      (values 'equals #\=))
		     (#\;
		      (values 'eol #\;))
		     ((lower-case c)
		      (values 'var (string c)))
		     ((upper-case c)
		      (push c chars)
		      (multiple-value-bind (word rest) (take-word chars)
			(setq chars rest)
			(values 'var word)))
		     ((whitespace _) (self))))))
      #'self)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parse-lambda (lambda var period terms)
    (declare (ignore lambda period))
    (mklambda var terms))

  (defun parse-assignment (var equals terms)
    (declare (ignore equals))
    (list :assign var terms))

  (defun parse-terms (term)
    (list :term term))

  (defun take-some (&rest idxs)
    (lambda (&rest arr)
      (loop for i in idxs collecting (nth i arr)))))

(defvar parser)
(define-parser parser
  (:muffle-conflicts t)
  (:start-symbol expressions)
  (:terminals (lparen rparen lambda period equals eol var))
  (:precedence ((:right comma)))

  (expressions
   (expression eol expressions (take-some 0 2))
   (expression eol (compose #'car #'list))
   expression)
  
  (expression
   (var equals terms #'parse-assignment)
   (terms #'parse-terms))
  
  (terms
   (terms term #'mkapp)
   term)
  
  (term
   (lparen terms rparen (compose #'cadr #'list))
   (lambda var period terms #'parse-lambda)
   var))

(defun decode-parsed (terms)
  (let (last-term env)
    (labels ((decode (terms)
	       (when terms
		 (ematch terms
		   ((list :assign var t1)
		    (when (assoc var env)
		      (error "variable already bound ~a" var))
		    (push (cons var t1) env))
		   ((list :term term)
		    (setq last-term term))
		   ((list t1 t2)
		    (decode t1)
		    (decode t2))))))
      (decode terms)
      (values last-term (nreverse env)))))

(defun parse (str)
  (decode-parsed
   (parse-with-lexer (make-lexer str) parser)))
