;;; cogen-record.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; definitions that implement records
;;; example:
;;; (define-record type-name (sel1 ...) aform1 ... )
;;; aform ::= asel | (asel exp)
;;; defines a record type with one constructor, make-[type-name], with
;;; arguments sel1, ..., seln and additional fields asel1, ... aseln
;;; the additional fields can have initial values specified with [exp]
;;; (the argument fields of the constructors are in scope when eval [exp] 
;;; selectors [type-name]->[sel1], ..., [type-name]->[asel1], ..., and
;;; update operations [type-name]->[sel1]! ... [type-name]->[asel1]!, ...
(define-syntax define-record
  (lambda (x r c)
    (let* ((%begin (r 'begin))
	   (%car (r 'car))
	   (%define (r 'define))
	   (%equal? (r 'equal?))
	   (%error (r 'error))
	   (%if (r 'if))
	   (%lambda (r 'lambda))
	   (%vector (r 'vector))
	   (%vector? (r 'vector?))
	   (%vector-ref (r 'vector-ref))
	   (%vector-set! (r 'vector-set!))
	   (type-name (cadr x))
	   (sel* (caddr x))
	   (rest (cdddr x))
	   (any->symbol
	    (lambda args
	      (string->symbol
	       (apply string-append
		      (map (lambda (arg)
			     (cond
			      ((symbol? arg) (symbol->string arg))
			      ((string? arg) arg)
			      ((number? arg) (number->string arg))))
			   args)))))
	   (rest->sym
	    (lambda (arg)
	      (if (pair? arg) (car arg) arg)))
	   (rest->init
	    (lambda (arg)
	      (if (pair? arg) (cadr arg) #f)))
	   (a-sel* (map rest->sym rest))
	   (a-init* (map rest->init rest)))
      `(,%begin
	(,%define ,(any->symbol "make-" type-name)
		  (,%lambda ,sel*
			    (,%vector ',type-name ,@sel* ,@a-init*)))
	(,%define ,(any->symbol type-name "?")
		  (,%lambda (rec)
			    (and (,%vector? rec)
				 (,%equal? (,%vector-ref rec 0) ',type-name))))
	,@(let loop ((sel* (append sel* a-sel*)) (i 1))
	    (if (null? sel*)
		'()
		(cons
		 `(,%define ,(any->symbol type-name "->" (car sel*))
		    (,%lambda (rec)
		      (,%if (,(any->symbol type-name "?") rec)
			    (,%vector-ref rec ,i)
			    (,%error "error: select ~d->~d"
				     ',type-name ',(car sel*)))))
		 (cons
		  `(,%define ,(any->symbol type-name "->" (car sel*) "!")
		     (,%lambda (rec arg)
		      (,%if (,(any->symbol type-name "?") rec)
			    (,%vector-set! rec ,i arg)
			    (,%error "error: set ~d->~d!"
				     ',type-name ',(car sel*)))))
		  (loop (cdr sel*) (+ i 1))))))))))
