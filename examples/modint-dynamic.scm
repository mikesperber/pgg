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
(define-without-memoization (jump-initial module label args)
  (_load module
	 (lambda (mod-name mod-body)
	   (let loop ((labels (map (lambda (def) (car def)) mod-body)))
	     (if (pair? labels)
		 (let ((this-label (car labels)))
		   (if (eqv? label this-label)
		       ((jump-global mod-name mod-body) this-label args)
		       (loop (cdr labels))))
		 (jump-initial module label args))))))

;;; main : 1 1 0 1
(define (main module label nregs initial_args)
  (let ((args (copy nregs initial_args)))
    (jump-initial module label args)))

