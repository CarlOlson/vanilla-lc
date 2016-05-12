(in-package :vanilla-lc)

(defun eval-string (str &key consts (timeout 1) (reduce t))
    (multiple-value-bind (term env) (parse str)
      (let* ((env  (process-env env consts))
	     (term (de-bruijn-index term))
	     (term (subst-with-env term env)))
	(sb-ext:with-timeout timeout
	  (if reduce (reduce-star term) term)))))

(defun eval-file (filename &key consts (timeout 1) (reduce t))
  (eval-string (file-text filename)
	       :consts consts :timeout timeout :reduce reduce))
