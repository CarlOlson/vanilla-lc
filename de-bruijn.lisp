(in-package :vanilla-lc)

;; NOTE attempt at removing mvb resulted in errors and slower code

(defun de-bruijn-index (term &optional stack)
  (ematch term
    ((guard (type string) ;; bound var
	    (member term stack :test 'string=))
     (position term stack :test 'string=))
    ((type string) term) ;; named or free var
    ((app t1 t2)
     (mkapp (de-bruijn-index t1 stack)
	    (de-bruijn-index t2 stack)))
    ((lambda bind t1)
     (mklambda1 (de-bruijn-index t1 (cons bind stack))))))

(defun de-bruijn-inc (term by &optional (level 0))
  (ematch term
    ((guard (type integer)
	    (< term level))
     ;; var's binding in original term
     (values term nil term))
    ((guard (type integer)
	    (>= term level))
     ;; var's binding outside original term
     (values (+ term by) nil (+ term by)))
    ((guard (lambda1 _ dirty depth)
    	    (<= depth level))
     (values term dirty depth))
    ((guard (app _ _ dirty depth)
    	    (< depth level))
     ;; NOTE during profiling this match did not reduce function calls
     (values term dirty depth))
    ((app t1 t2)
     (multiple-value-bind (t1 t1-dirty t1-depth)
	 (de-bruijn-inc t1 by level)
       (multiple-value-bind (t2 t2-dirty t2-depth)
	   (de-bruijn-inc t2 by level)
	 (let ((dirty (or t1-dirty t2-dirty (type-lambda1-p t1)))
	       (depth (max t1-depth t2-depth)))
	   (values (mkapp t1 t2 dirty depth) dirty depth)))))
    ((lambda1 t1)
     (multiple-value-bind (t1 dirty depth)
	 (de-bruijn-inc t1 by (1+ level))
     (values (mklambda1 t1 dirty depth) dirty (max 0 (1- depth)))))))

(defun de-bruijn-apply (term subst &optional (level 0))
  ;; LEVEL is how many lambda terms deep we are in our traversal
  ;; DEPTH is for the depth slot of lambda terms
  (ematch term
    ((guard (type integer)
	    (< term level))
     ;; var's binding in original term
     (values term nil term))
    ((guard (type integer)
	    (= term level))
     ;; var to be substituted
     (multiple-value-bind (term dirty depth)
	 (de-bruijn-inc subst level)
       (values term dirty depth)))
    ((guard (type integer)
	    (> term level))
     ;; var's binding outside original term
     (values (1- term) nil (1- term)))
    ((guard (app _ _ dirty depth)
    	    (< depth level))
     ;; subterms can't contain substitutions
     (values term dirty depth))
    ((guard (lambda1 _ dirty depth)
    	    (<= depth level))
     ;; subterms can't contain substitutions
     (values term dirty depth))
    ((app t1 t2)
     (multiple-value-bind (t1 t1-dirty t1-depth)
     	 (de-bruijn-apply t1 subst level)
       (multiple-value-bind (t2 t2-dirty t2-depth)
     	   (de-bruijn-apply t2 subst level)
     	 (let ((dirty (or t1-dirty t2-dirty (type-lambda1-p t1)))
     	       (depth (max t1-depth t2-depth)))
	   (values (mkapp t1 t2 dirty depth) dirty depth)))))
    ((lambda1 t1)
     (multiple-value-bind (t1 dirty depth)
     	 (de-bruijn-apply t1 subst (1+ level))
       (values (mklambda1 t1 dirty depth) dirty (max 0 (1- depth)))))))

(defun reduce-some (term)
  (let (reduced)
    (labels
	((self (term)
	   (ematch term
	     ((type integer)            (values term nil term))
	     ((lambda1 _ (^ nil) depth) (values term nil depth))
	     ((lambda1 t1)
	      (multiple-value-bind (t1 dirty depth) (self t1)
		(values (mklambda1 t1 dirty depth) dirty (max 0 (1- depth)))))
	     ((app (lambda1 t1) t2)
	      (setq reduced t)
	      (multiple-value-bind (term dirty depth)
		  (de-bruijn-apply t1 t2)
		(values term dirty depth)))
	     ((app  _  _ (^ nil) depth) (values term nil depth))
	     ((app t1 t2)
	      (multiple-value-bind (t1 t1-dirty t1-depth) (self t1)
		(multiple-value-bind (t2 t2-dirty t2-depth) (self t2)
		  (let ((dirty (or t1-dirty t2-dirty (type-lambda1-p t1)))
			(depth (max t1-depth t2-depth)))
		    (values (mkapp t1 t2 dirty depth)
			    dirty depth))))))))
      (values (self term) reduced))))

(defun reduce-star (term)
  (multiple-value-bind (term reduced) (reduce-some term)
    (if reduced
	(reduce-star term)
	term)))

(defun subst-with-env (term env)
  (ematch term
    ((guard (type string)
	    (gethash term env))
     (gethash term env))
    ((app t1 t2)
     (mkapp (subst-with-env t1 env)
	    (subst-with-env t2 env)))
    ((lambda1 term)
     (mklambda1 (subst-with-env term env)))
    ((type integer) term)
    ((type string)
     (error "undefined variable ~a" term))))

(defun process-env (env &optional native)
  (let ((env-hash (make-hash-table :test #'equal)))
    (labels ((convert (term)
	       (subst-with-env (de-bruijn-index term) env-hash)))
      (loop
	 for (name . fn) in native
	 do (setf (gethash name env-hash) fn))
      (loop
	 for (name . term) in env
	 do (setf (gethash name env-hash) (convert term)))
      env-hash)))
