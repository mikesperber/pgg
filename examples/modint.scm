;;; very simple modular interpreter
;;; requires modules in dependency order, main module first

;;; (load "modint-base.scm")

;;; jump : 0 1 0 [1]0
(define (jump-local modulename-of mod)
  (lambda (name args)
    (let ((found (assoc name mod)))
      (if found
	  (_memo (exec (jump-local modulename-of mod)
		       (cdr found)
		       args))
	  (jump-global modulename-of name args)))))

(define (jump-global modulename-of name args)
  (access (modulename-of name)
	  (lambda (mod-name mod-body)
	    (if (null? mod-body)
		(error "Undefined label")
		((jump-local modulename-of mod-body) name args)))))

;;; main : 1 0 0 1
(define (main modulename-of name nargs initial_args)
  (let ((args (copy nargs initial_args)))
    (jump-global modulename-of name args)))
