;;; an improved modular interpreter
;;; recursive modules

;;; (load "modint-base.scm")

;;; jump-global : 0 0 0 1
(define (jump exported-names modname) 
  (lambda (name args)
    (let* ((is-exported (assoc name exported-names))
	   (name+mod (or is-exported (cons name modname))))
      (access (cdr name+mod)
	      (lambda (this-modname this-mod)
		(let ((found (assoc name this-mod)))
		  (if found 
		      (exec (jump exported-names 
				  this-modname)
			    (cdr found)
			    args)
		      (error "Undefined name"))))))))

;;; main : 0 1 0 1
(define-without-memoization (main exported-names name 
				  nargs initial_args)
  (let ((args (copy nargs initial_args)))
    (let loop ((exports exported-names))
      (if (null? exports)
	  (dyn-error "Unknown name")
	  (let ((export (car exports)))
	    (if (eqv? name (car export))
		((jump exported-names 'noname) 
		 (car export) args)
		(loop (cdr exports))))))))

