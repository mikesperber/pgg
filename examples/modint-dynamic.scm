;;; sophisticated interpreter
;;; no declarations, no restrictions

;;; (load "modint-base.scm")

;;; jump-global : 0 0 0 [1]0
(define (jump-global mod-body)
  (lambda (name args)
    (cond
     ((pair? name)
      (let ((req-modname (car name)) (req-name (cdr name)))
	(_load req-modname
	       (lambda (this-modname this-body)
		 (let ((found (assoc req-name this-body)))
		   (exec (jump-global this-body)
			 (cdr found)
			 args))))))
     ((assoc name mod-body)
      => (lambda (found)
	   (_memo (exec (jump-global mod-body)
			(cdr found)
			args))))
     (else
      (error "Undefined label")))))

;;; jump-initial: 1 1 1
(define-without-memoization (jump-initial modulename name args)
  (_load 
   modulename
   (lambda (mod-name mod-body)
     (if (null? mod-body)
	 (dyn-error "invalid module name")
	 (let loop ((names (map (lambda (def) (car def)) mod-body)))
	   (if (null? names)
	       (jump-initial modulename name args)
	       (let ((this-name (car names)))
		 (if (eqv? name this-name)
		     ((jump-global mod-body) this-name args)
		     (loop (cdr names))))))))))

;;; main : 1 1 0 1
(define (main modulename name nargs initial_args)
  (let ((args (copy nargs initial_args)))
    (jump-initial modulename name args)))
    

