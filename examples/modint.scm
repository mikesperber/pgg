;;; very simple modular interpreter
;;; requires modules in dependency order, main module first

;;; (load "modint-base.scm")

;;; jump : 0 1 0 [1]0
(define (jump-local module-of mod name args)
  (let ((found (assoc name mod)))
    (if found
	(_memo (exec (lambda (name args) (jump-local module-of mod name args))
		     (cdr found)
		     args))
	(jump-global module-of name args))))

(define (jump-global module-of name args)
  (_load (module-of name)
	 (lambda (mod-name mod-body)
	   (if (null? mod-body)
	       (error "Undefined label")
	       (jump-local module-of mod-body name args)))))

;;; main : 1 0 0 1
(define (main module-of name nargs initial_args)
  (let ((args (copy nargs initial_args)))
    (jump-global module-of name args)))

