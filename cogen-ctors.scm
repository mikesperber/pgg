;;; cogen-ctors.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; definitions that implement constructors
;;; example:
;;; (define-data type-name (nil) (cons car cdr))
;;; defines a datatype with two constructors, the nullary "nil" and
;;; the binary "cons", the selectors of the latter being "car" and "cdr"
(define-syntax define-data
  (lambda (x r c)
    (let* ((%begin (r 'begin))
	   (%car (r 'car))
	   (%define (r 'define))
	   (%equal? (r 'equal?))
	   (%if (r 'if))
	   (%list (r 'list))
	   (%list-ref (r 'list-ref))
	   (rest (cddr x))
	   (ctor-decls (if (pair? (car rest)) rest (cdr rest))))
      (cons %begin
	    (apply append
		   (map (lambda (ctor-decl)
			  (let* ((ctor-name (car ctor-decl))
				 (ctor-test (string->symbol (string-append
							     (symbol->string
							      ctor-name) "?"))))
			    (cons
			     `(,%define
			       ,ctor-decl
			       (,%list ',(car ctor-decl)
				       ,@(cdr ctor-decl)))
			     (cons
			      `(,%define (,ctor-test arg)
					 (,%equal? (,%car arg) ',ctor-name))
			      (let loop ((sels (cdr ctor-decl)) (i 1))
				(if (null? sels)
				    '()
				    (cons
				     `(,%define
				       (,(car sels) x)
				       (,%if (,ctor-test x)
					     (,%list-ref x ,i)
					     (error "bad selector ~A applied to ~S" ',(car sels) x)))
				     (loop (cdr sels) (+ i 1)))))))))
			ctor-decls))))))
;;;
;;;(define-syntax defctor
;;;  (lambda (x r c)
;;;    `(,(r 'define)
;;;      ,(cadr x)
;;;      (,(r 'list) ,@(cddr x))))) 
;;;
;;; define-syntax does not work as a single expression must expand to
;;; another single expression
;;; no there was a format error, furthermore the form (begin <define>
;;; ...) is explicitly allowed for (essential syntax)!

(define (ctors-make-test ctor)
  (string->symbol (string-append (symbol->string ctor) "?")))

(define (ctors-generate-define defconstr-clause)
  (map (lambda (ctor-decl)
	 (cons
	  `(define ,ctor-decl (list ',(car ctor-decl) ,@(cdr ctor-decl)))
	  (let loop ((sels (cdr ctor-decl)) (i 1))
	    (if (null? sels)
		'()
		(cons
		 `(define (,(car sels) x) (list-ref x ,i))
		 (loop (cdr sels) (+ i 1)))))))
       (cdr defconstr-clause))) 