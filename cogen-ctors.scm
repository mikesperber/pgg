;;; cogen-ctors
;;; definitions that implement constructors
;;; example:
;;; (defconstr (nil) (cons car cdr))
;;; defines a datatype with two constructors, the nullary "nil" and
;;; the binary "cons", the selectors of the latter being "car" and "cdr"
(define-syntax defconstr
  (lambda (x r c)
    (cons (r 'begin)
	  (map (lambda (ctor-decl)
		 (cons
		  `(,(r 'define)
		    ,ctor-decl
		    (,(r 'list) ',(car ctor-decl)
				,@(cdr ctor-decl)))
		  (let loop ((sels (cdr ctor-decl)) (i 1))
		    (if (null? sels)
			'()
			(cons
			 `(,(r 'define)
			   (,(car sels) x)
			   (,(r 'list-ref) x ,i))
			 (loop (cdr sels) (+ i 1)))))))
	       (cdr x)))))
;;; gives error:
;;; Error: exception
;;;        (record-ref ':definition 1)
;;;
;;;(define-syntax defctor
;;;  (lambda (x r c)
;;;    `(,(r 'define)
;;;      ,(cadr x)
;;;      (,(r 'list) ,@(cddr x))))) 
;;;
;;; define-syntax does not work as a single expression must expand to
;;; another single expression

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