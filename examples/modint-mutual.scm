;;; an improved modular interpreter
;;; recursive modules

;;; (load "modint-base.scm")

;;; jump-global : 0 0 0 [1]0
(define (jump-global exported-names mod) 
  (lambda (name args)
    (cond
     ((assoc name mod)
      => (lambda (found)
	   (_memo (exec (jump-global exported-names mod)
			(cdr found)
			args))))
     ((assoc name exported-names)
      => (lambda (name+mod)
	   (_load (cdr name+mod)
		  (lambda (mod-name this-mod)
		    (let ((found (assoc name this-mod)))
		      (exec (jump-global exported-names 
					 this-mod)
			    (cdr found)
			    args))))))
     (else
      (error "Undefined name")))))

;;; main : 0 1 0 1
(define-without-memoization (main exported-names name 
				  nargs initial_args)
  (let ((args (copy nargs initial_args)))
    (let loop ((exports exported-names))
      (if (null? exports)
	  (dyn-error "Unknown name")
	  (let ((export (car exports)))
	    (if (eqv? name (car export))
		((jump-global exported-names '()) 
		 (car export) args)
		(loop (cdr exports))))))))

