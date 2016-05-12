(in-package :vanilla-lc)

(defpattern lower-case (c)
  (let ((it (gensym)))
    `(guard1 (,it :type character)
	     (lower-case-p ,it)
	     ,it ,c)))

(defpattern upper-case (c)
  (let ((it (gensym)))
    `(guard1 (,it :type character)
	     (upper-case-p ,it)
	     ,it ,c)))

(defpattern whitespace (c)
  (let ((it (gensym)))
    `(guard1 (,it :type character)
	     (member ,it '(#\Newline #\Tab #\Return #\ ))
	     ,it ,c)))

(defpattern ^ (var)
  (let ((it (gensym)))
    `(guard1 ,it (equalp ,it ,var))))

(defun mklambda (bind term)
  (list :lambda bind term))

(defpattern lambda (bind term)
  `(list* :lambda ,bind ,term _))

(defstruct (type-lambda1
	     (:constructor mklambda1 (t1 &optional dirty depth)))
  t1
  (dirty t :type boolean)
  (depth most-positive-fixnum :type fixnum))

(defpattern lambda1 (t1 &optional dirty depth)
  `(type-lambda1 :t1 ,t1
		 ,@(if dirty (list :dirty dirty))
		 ,@(if depth (list :depth depth))))

(defstruct (type-app
	     (:constructor mkapp (t1 t2 &optional dirty depth)))
  t1 t2
  (dirty t :type boolean)
  (depth most-positive-fixnum :type fixnum))

(defpattern app (t1 t2 &optional dirty depth)
  `(type-app :t1 ,t1 :t2 ,t2
	     ,@(if dirty (list :dirty dirty))
	     ,@(if depth (list :depth depth))))

(defun string-chars (str)
  (loop for c across str collecting c))

(defun take-word (chars &optional acc)
  (labels ((rconcat (chars)
	   (concatenate 'string (nreverse chars))))
    (cond
      ((null chars)
       (values (rconcat acc) nil))
      ((alphanumericp (car chars))
       (take-word (cdr chars)
		  (cons (car chars) acc)))
      (t (values (rconcat acc) chars)))))

(defun file-text (filename)
  (iterate (for c in-file filename using 'read-char)
	   (collect c into chars)
	   (finally (return-from file-text (concatenate 'string chars)))))

