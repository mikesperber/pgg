;;; sophisticated interpreter
;;; no declarations, no restrictions

;;; (load "modint-base.scm")

;;; jump-global : 0 0 0 [1]0
(define (jump-global mod-name mod-body)
  (lambda (name args)
    (cond
     ((pair? name)
      (let ((req-mod (car name)) (req-label (cdr name)))
	(_load req-mod
	       (lambda (this-name this-body)
		 (let ((found (assoc req-label this-body)))
		   (exec (jump-global this-name this-body)
			 (cdr found)
			 args))))))
     ((assoc name mod-body)
      => (lambda (found)
	   (_memo (exec (jump-global mod-name mod-body)
			(cdr found)
			args))))
     (else
      (error "Undefined label")))))

;;; jump-initial: 1 1 [1]0
(define-without-memoization (jump-initial modulename name args)
  (_load modulename
	 (lambda (mod-name mod-body)
	   (if (null? mod-body)
	       (dyn-error "invalid module name")
	       (let loop ((names (map (lambda (def) (car def)) mod-body)))
		 (if (pair? names)
		     (let ((this-name (car names)))
		       (if (eqv? name this-name)
			   ((jump-global mod-name mod-body) this-name args)
			   (loop (cdr names))))
		     (jump-initial modulename name args)))))))

;;; main : 1 1 0 1
(define (main modulename name nargs initial_args)
  (let ((args (copy nargs initial_args)))
    (jump-initial modulename name args)))
    

