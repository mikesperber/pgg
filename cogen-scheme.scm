;;; cogen-scheme

;;; convert side-effect free scheme to abstract syntax
;;; + translate and, or to if
;;; + simplify let to (let ((V E)) E) 
;;; + eta-expand procedures and
;;; - operators which occur as variables
;;; + add let binders for every parameter of a procedure

;;; construct a symbol table which maps variables  to one of
;;; annMakeOp, annMakeCall, or annMakeApp
(define (scheme->abssyn-d d*)
  (let ((symtab
	 (map (lambda (d)
		(list (caadr d) annMakeCall (length (cdadr d)))) d*)))
    (map (lambda (d)
	   (let* ((formals (cdadr d))
		  (symtab (append (map (lambda (v)
					 (list v annMakeApp -1)) formals)
				  symtab)))
	     (annMakeDef (caadr d)
			 formals
			 (scheme->abssyn-wrap-in-let
			  formals
			  (scheme->abssyn-e (caddr d) symtab)))))
	 d*))) 

(define (scheme->abssyn-e e symtab)
  (let loop ((e e))
    (cond
     ((symbol? e)
      (let ((syminfo (assoc e symtab)))
	(if (and syminfo (>= (caddr syminfo) 0))
	    (let* ((arity (caddr syminfo))
		   (newvars (let loop ((i arity))
			      (if (= i 0)
				  '()
				  (cons (gensym 'XXX)
					(loop (- i 1)))))))
	      (loop `(LAMBDA (,@newvars) (,e ,@newvars))))
	    (annMakeVar e))))
     ((not (pair? e))
      (annMakeConst e))
     ((equal? (car e) 'quote)
      (annMakeConst (cdr e)))
     (else
      (let* ((tag (car e))
	     (args (cdr e))
	     (mkproc (assoc tag symtab)))
	(if mkproc
	    (apply (cadr mkproc) (list tag (map loop args)))
	    (cond
	     ((equal? tag 'IF)
	      (annMakeCond (loop (car args))
			   (loop (cadr args))
			   (loop (caddr args))))
	     ((equal? tag 'AND)
	      (cond
	       ((null? args)
		(annMakeConst #t))
	       ((= 1 (length args))
		(loop (car args)))
	       (else
		(loop `(IF ,(car args) (AND ,@(cdr args)) #f)))))
	     ((equal? tag 'OR)
	      (cond
	       ((null? args)
		(annMakeConst #f))
	       ((= 1 (length args))
		(loop (car args)))
	       (else
		(let ((newvar (gensym 'XXX)))
		  (loop `(LET ((,newvar ,(car args)))
			   (IF ,newvar ,newvar (OR ,@(cdr
						      args)))))))))
	     ((equal? tag 'LET)
	      (let ((headers (car args))
		    (body (cadr args)))
		(cond
		 ((null? headers)
		  (loop body))
		 ((= 1 (length headers))
		  (annMakeLet (caaar args)
			      (cadaar args)
			      (scheme->abssyn-e
			       body
			       (cons (list (caaar args) annMakeApp -1)
				     symtab)))) 
		 (else
		  (let ((newvars (map (lambda (x) (gensym 'XXX))
				      headers))
			(f1 (lambda (nv hdr)
			      `(,nv ,(cadr hdr))))
			(f2 (lambda (nv hdr)
			      `(,(car hdr) ,nv))))
		    (loop
		     '(LET* (,@(map f1 newvars headers)
			     ,@(map f2 newvars headers))
			,body)))))))
	     ((equal? tag 'LET*)
	      (let ((headers (car args))
		    (body (cadr args)))
		(cond
		 ((null? headers)
		  (loop body))
		 (else
		  (annMakeLet (caar args)
			      (cadar args)
			      (scheme->abssyn-e
			       `(LET* (,(cdr headers))
				  ,body)
			       (cons (list (caaar args) annMakeApp -1)
				     symtab)))))))
	     ((equal? tag 'LAMBDA)
	      (let ((symtab
		     (append (map (lambda (var) (list var annMakeApp -1))
				  (car args))
			     symtab)))
		(annMakeLambda (car args)
			       (scheme->abssyn-e (cadr args)
						 symtab))))
	     (else
	      (annMakeOp tag (map loop args))))))))))

;;; wrap the formal parameters in let-binders around a procedure body 
(define (scheme->abssyn-wrap-in-let formals body)
  (let loop ((formals formals))
    (if (null? formals)
	body
	(let ((v (car formals)))
	  (annMakeLet v (annMakeVar v) (loop (cdr formals)))))))

;;; process (define-type t (c1 s11 ... s1n) ... (cm sm1 ... smn))
;;; a constructor description for ci is a list
;;; (t np nc nt)
;;; where np = sum of # args of c[1] ... c[i-1]
;;;       nc = # args of c[i]
;;;       nt = sum of # args of c[1] ... c[m]
(define (desc-type desc) (list-ref desc 0))
(define (desc-np desc) (list-ref desc 1))
(define (desc-nc desc) (list-ref desc 2))
(define (desc-nt desc) (list-ref desc 3))
(define (scheme->abssyn-nc ctor)
  (- (length ctor) 1))
(define (scheme->abssyn-nt ctors)
  (apply + (map scheme->abssyn-nc ctors)))
(define (scheme->abssyn-define-type dc*)
  (apply append
	 (map (lambda (dc)
		(let* ((type-name (cadr dc))
		       (ctors (cddr dc))
		       (nt (scheme->abssyn-nt ctors)))
		  (let loop ((ctors ctors) (np 0))
		    (if (null? ctors)
			'()
			(let* ((ctor (car ctors))
			       (nc (scheme->abssyn-nc ctor)))
			  (append (scheme->abssyn-one-ctor
				   (list type-name np nc nt))
				  (loop (cdr ctors) (+ np nc)))))))) dc*)))
(define (scheme->abssyn-one-ctor desc ctor)
  (let* ((the-ctor (car ctor))
	 (the-test (string->symbol
		    (string-append (symbol->string the-ctor) "?")))
	 (selectors (cdr ctor)))
  (cons
   (cons the-ctor (annMakeCtor1 desc))
   (cons
    (cons the-test (annMakeTest1 ctor desc))
    (let loop ((selectors selectors) (i 1))
      (if (null? selectors)
	  '()
	  (cons
	   (cons (car selectors) (annMakeSel1 the-ctor desc i))
	   (loop (cdr selectors) (+ i 1))))))))) 

