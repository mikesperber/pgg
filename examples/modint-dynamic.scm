;;; sophisticated interpreter
;;; no declarations, no restrictions

;;; (load "modint-base.scm")

;;; jump : 0 0 1
(define (jump mod+name args)
  (access (car mod+name)
	  (lambda (this-modname this-mod)
	    (let ((found (assoc (cdr mod+name) this-mod)))
	      (if found 
		  (exec jump (cdr found) args)
		  (error "Undefined name"))))))
 
;;; jump-initial: 1 1 1
(define (jump-initial modulename name args)
  (access
   modulename
   (lambda (mod-name mod-body)
     (let loop ((names (map (lambda (def) (car def)) mod-body)))
       (if (null? names)
	   (dyn-error "unknown name")
	   (let ((this-name (car names)))
	     (if (eqv? name this-name)
		 (jump  (cons mod-name this-name) args)
		 (loop (cdr names)))))))))

;;; main : 1 1 0 1
(define (main modulename name nargs initial_args)
  (let ((args (copy nargs initial_args)))
    (jump-initial modulename name args)))
    

