;;; cogen-completer.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

(define-syntax _complete
  (syntax-rules ()
    ((_complete body)
     (let ((var (gensym-local 'mlet)))
       (shift k (make-residual-let-trivial var body (k var)))))))

(define-syntax _complete-serious
  (syntax-rules ()
    ((_complete-serious body)
     (let ((var (gensym-local 'mlet)))
       (shift k (make-residual-let-serious var body (k var)))))))

(define-syntax _complete-maybe
  (syntax-rules ()
    ((_complete-serious body)
     (let ((value body))
       (if (or (symbol? value) (number? value))
	   value
	   (let ((var (gensym-local 'mlet)))
	     (shift k (make-residual-let-serious var value (k var)))))))))

