;;; cogen-completer.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

(define (_complete body)
  (let ((var (make-residual-variable (gensym-local 'mlet))))
    (shift k (make-residual-let-trivial var body (k var)))))

(define (_complete-no-result body)
  (let ((var (make-residual-variable (gensym-local 'mlet))))
    (shift k (make-residual-begin body (k var)))))

(define (_complete-serious proc args)
  (let ((var (make-residual-variable (gensym-local 'mlet))))
    (shift k (make-residual-let-serious
	      var proc args 
	      (k var)))))

(define (_complete-serious-apply proc arg)
  (let ((var (make-residual-variable (gensym-local 'mlet))))
    (shift k (make-residual-let-serious-apply var proc arg
					      (k var)))))

(define (_complete-serious-no-result proc args)
  (let ((var (make-residual-variable (gensym-local 'mlet))))
    (shift k (make-residual-begin (apply make-residual-call proc args)
				  (k var)))))

(define (_complete-serious-apply-no-result proc arg)
  (let ((var (make-residual-variable (gensym-local 'mlet))))
    (shift k (make-residual-begin (make-residual-apply proc arg)
				  (k var)))))

(define (_complete-maybe body)
  (let ((value body))
    (if (or (symbol? value) (number? value))
	value
	(let ((var (make-residual-variable (gensym-local 'mlet))))
	  (shift k (make-residual-let-serious var (car value) (cdr value)
					      (k var)))))))

