;;; sophisticated interpreter
;;; no declarations, no restrictions

;;; (load "modint-base.scm")

;;; jump : 0 0 1
(define (jump mod+name args)
  (if (pair? mod+name)
      (_load (car mod+name)
	     (lambda (this-modname this-mod)
	       (let ((found (assoc (cdr mod+name) this-mod)))
		 (if found 
		     (exec jump (cdr found) args)
		     (error "Undefined name")))))
      (error "Unqualified name")))
 
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
		     (jump  (cons mod-name this-name) args)
		     (loop (cdr names))))))))))

;;; main : 1 1 0 1
(define (main modulename name nargs initial_args)
  (let ((args (copy nargs initial_args)))
    (jump-initial modulename name args)))
    

