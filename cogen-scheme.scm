;;; cogen-scheme

;;; convert side-effect free scheme to abstract syntax
;;; + translate and, or to if
;;; + simplify let to (let ((V E)) E) 
;;; + eta-expand procedures and
;;; - operators which occur as variables
;;; + add let binders for every parameter of a procedure
;;; + add letrec and lambda lifting
;;; + handle begin

;;; third step: transform to internal form

;;; construct a symbol table which maps variables  to one of
;;; annMakeOp, annMakeCall, annMakeApp, or annMakeCtor by extending
;;; the constructor symbol table `ctor-symtab'
;;; remove AND, OR, and BEGIN forms
(define *scheme->abssyn-label-counter* 0)
(define (scheme->abssyn-make-label)
  (set! *scheme->abssyn-label-counter*
	(+ 1 *scheme->abssyn-label-counter*))
  *scheme->abssyn-label-counter*)
(define (scheme->abssyn-d d* ctor-symtab)
  ;;(display "scheme->abssyn") (newline)
  (set! *scheme->abssyn-label-counter* 0)
  (let* ((d* (scheme-rename-variables-d d*))
	 (d* (scheme-lambda-lift-d d*))
	 (symtab
	  (append
	   (map (lambda (d)
		  (list (caadr d) annMakeCall (length (cdadr d)))) d*)
	   ctor-symtab)))
    (map (lambda (d)
	   (let* ((formals (cdadr d))
		  (symtab (append (map (lambda (v)
					 (list v scheme-make-var-app -1)) formals)
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
		   (newvars (nlist arity (lambda () (gensym 'XXX)))))
	      (loop `(LAMBDA (,@newvars) (,e ,@newvars))))
	    (annMakeVar e))))
     ((not (pair? e))
      (annMakeConst e))
     ((equal? (car e) 'QUOTE)
      (annMakeConst (cadr e)))
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
			      (scheme->abssyn-e
			       (cadaar args)
			       symtab)
			      (scheme->abssyn-e
			       body
			       (cons (list (caaar args) scheme-make-var-app -1)
				     symtab))))
		 ;; the following case is dead
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
	     ;; the LET* case is dead
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
			       (cons (list (caaar args) scheme-make-var-app -1)
				     symtab)))))))
	     ;; misuse LET for sequencing, too
	     ;; these LETs may have to be marked as not discardable if
	     ;; side effects are around
	     ((equal? tag 'BEGIN)
	      (scheme->abssyn-e
	       (let loop ((args args))
		 (if (= 1 (length args))
		     (car args)
		     `(LET ((,(gensym 'BEGIN) ,(car args)))
			,(loop (cdr args)))))
	       symtab))
	     ((equal? tag 'LAMBDA)
	      (let* ((formals (car args))
		     (symtab (append
			      (map (lambda (var) (list var scheme-make-var-app -1))
				   formals)
			      symtab)))
		(annMakeLambda
		 (scheme->abssyn-make-label)
		 (car args)
		 (scheme->abssyn-wrap-in-let formals
					     (scheme->abssyn-e (cadr args)
							       symtab)))))
	     ((pair? tag)
	      (annMakeApp (loop tag) (map loop args)))
	     (else
	      (annMakeOp tag (map loop args))))))))))

;;; wrap the formal parameters in let-binders around a procedure body 
(define (scheme->abssyn-wrap-in-let formals body)
  (let loop ((formals formals))
    (if (null? formals)
	body
	(let ((v (car formals)))
	  (annMakeLet v (annMakeVar v) (loop (cdr formals)))))))

(define (scheme-make-var-app var rands)
  (annMakeApp (annMakeVar var) rands))

;;; first step: rename variables so that every variable has exactly
;;; one binding occurrence, transform LET* and LET into LET with just
;;; a single binder, COND into IF, introduce BEGIN
(define *scheme-rename-counter* '())
(define (scheme-rename-clone var)
  (let* ((var-counter
	  (or (assoc var *scheme-rename-counter*)
	      (begin (set! *scheme-rename-counter*
			   (cons (cons var 0) *scheme-rename-counter*))
		     (car *scheme-rename-counter*)))))
    (set-cdr! var-counter (+ 1 (cdr var-counter)))
    (string->symbol
     (string-append
      (symbol->string var) "_" (number->string (cdr var-counter))))))
(define (scheme-rename-variables-d d*)
  (set! *scheme-rename-counter* '())
  (let ((symtab (map (lambda (d) (cons (caadr d) (caadr d))) d*)))
    (map (lambda (d)
	   ;;(display "scheme-rename-variables: ") (display (caadr d)) (newline)
	   (let* ((fname (caadr d))
		  (formals (cdadr d))
		  (body-list (cddr d))
		  (body (if (= 1 (length body-list))
			    (car body-list)
			    `(BEGIN ,@body-list)))
		  (new-formals (map scheme-rename-clone formals))
		  (new-symtab (append (map cons formals new-formals)
				      symtab))
		  (new-body (scheme-rename-variables new-symtab body)))
	   `(DEFINE (,fname ,@new-formals) ,new-body))) d*)))

(define (scheme-rename-variables symtab e)
  (if (symbol? e)
      (let ((found (assoc e symtab)))
	(if found
	    (cdr found)
	    e))
      (if (not (pair? e))
	  e
	  (let ((tag (scheme-rename-variables symtab (car e)))
		(args (cdr e)))
	    (cond
	     ((equal? tag 'QUOTE)
	      e)
	     ;;
	     ((equal? tag 'COND)
	      (scheme-rename-variables
	       symtab
	      (let loop ((body args))
		  (cond
		   ((null? body)
		    #f)
		   ((equal? (caar body) 'ELSE)
		    (scheme-body-list->body (cdar body)))
		   (else				;general case
		    `(IF ,(caar body)
			 ,(scheme-body-list->body (cdar body))
			 ,(loop (cdr body))))))))
	     ;;
	     ((and (equal? tag 'LET)
		   (not (pair? (car args))))
	      (scheme-rename-variables
	       symtab
	      (let* ((name (car args))
		     (formals (map car (cadr args)))
		     (inits (map cadr (cadr args)))
		     (body-list (cddr args)))
		`(LETREC ((,name
			   (LAMBDA ,formals ,@body-list)))
		   (,name ,@inits)))))
	     ;;
	     ((equal? tag 'LET)
	      (let* ((formals (map car (car args)))
		     (bodies (map cadr (car args)))
		     (body-list (cdr args))
		     (new-formals (map scheme-rename-clone formals))
		     (new-bodies (map (lambda (body)
					(scheme-rename-variables
					 symtab body)) bodies))
		     (new-symtab (append (map cons formals new-formals)
				      symtab))
		     (new-body (scheme-rename-variables
				new-symtab
				(scheme-body-list->body body-list))))
		(let loop ((new-formals new-formals)
			   (new-bodies new-bodies))
		  (if (null? new-formals)
		      new-body
		      `(LET ((,(car new-formals) ,(car new-bodies)))
			 ,(loop (cdr new-formals) (cdr
						   new-bodies)))))))
	     ;;
	     ((equal? tag 'LET*)
	      (let* ((formals (map car (car args)))
		     (bodies (map cadr (car args)))
		     (body-list (cdr args))
		     (new-formals (map scheme-rename-clone formals)))
		(let loop ((formals formals)
			   (new-formals new-formals)
			   (bodies bodies)
			   (symtab symtab))
		  (if (null? formals)
		      (scheme-rename-variables
		       symtab
		       (scheme-body-list->body body-list))
		      `(LET ((,(car new-formals)
			      ,(scheme-rename-variables symtab (car
								bodies))))
			 ,(loop (cdr formals)
				(cdr new-formals)
				(cdr bodies)
				(cons (cons (car formals) (car
							   new-formals))
				      symtab)))))))
	     ;;
	     ((equal? tag 'LETREC)
	      (let* ((formals (map car (car args)))
		     (bodies (map cadr (car args)))
		     (body-list (cdr args))
		     (new-formals (map scheme-rename-clone formals))
		     (new-symtab (append (map cons formals new-formals)
				      symtab))
		     (new-bodies (map (lambda (formal body)
					;;(display "letrec: ") (display formal) (newline)
					(scheme-rename-variables
					 new-symtab body)) formals bodies))
		     (new-body (scheme-rename-variables
				new-symtab
				(scheme-body-list->body body-list))))
		`(LETREC ,(map list new-formals new-bodies)
		   ,new-body)))
	     ;;
	     ((equal? tag 'LAMBDA)
	      (let* ((formals (car args))
		     (body-list (cdr args))
		     (body (scheme-body-list->body body-list))
		     (new-formals (map scheme-rename-clone formals))
		     (new-symtab (append (map cons formals new-formals)
				      symtab))
		     (new-body (scheme-rename-variables new-symtab body)))
		`(LAMBDA ,new-formals ,new-body)))
	     (else
	      (cons tag (map (lambda (e) (scheme-rename-variables
					  symtab e)) args))))))))

(define (scheme-body-list->body body-list)
  (if (= 1 (length body-list))
      (car body-list)
      `(BEGIN ,@body-list)))

;;; second step: perform lambda lifting for LETREC forms, assumes
;;; first step has been performed
(define *scheme-lambda-lift-definitions* '())
(define (scheme-lambda-lift-add-definition d)
  (set! *scheme-lambda-lift-definitions*
	(cons d *scheme-lambda-lift-definitions*)))
(define (scheme-lambda-lift-d d*)
  (set! *scheme-lambda-lift-definitions* '())
  (let ((old-d* (map (lambda (d)
		       `(DEFINE ,(cadr d)
			  ,(scheme-lambda-lift (caddr d) (cdadr d))))
		     d*)))
    (append old-d* *scheme-lambda-lift-definitions*)))

;;; scheme-lambda-lift lifts all LETREC expressions
(define (scheme-lambda-lift e vars)
  (if (not (pair? e))
      e
      (let ((tag (car e))
	    (args (cdr e)))
	(cond
	 ((pair? tag)
	  (let ((rator (scheme-lambda-lift tag vars))
		(rands (map (lambda (e) (scheme-lambda-lift e vars))
			    args)))
	    `(,rator ,@rands)))
	 ((equal? tag 'QUOTE)
	  e)
	 ;; only one binder in a LET
	 ((equal? tag 'LET)
	  (let* ((header (caar args))
		 (body (cadr args))
		 (bound-var (car header))
		 (bound-body (cadr header)))
	    `(LET ((,bound-var
		    ,(scheme-lambda-lift bound-body vars)))
	       ,(scheme-lambda-lift body (cons bound-var vars)))))
	 ;;
	 ((equal? tag 'LETREC)
	  (let* ((headers (car args))
		 (body (cadr args))
		 (bound-vars (map car headers))
		 ;;(new-vars (append bound-vars vars))
		 (fv* (apply
		       set-union*
		       (map (lambda (h)
			      (scheme-freevars (cadr h) vars))
			    headers)))
		 (bound-bodies
		  (map (lambda (h)
			 (scheme-lambda-lift
			  (scheme-lambda-lift-vars bound-vars fv*
						   (cadr h))
			  vars))
		       headers)))
	    (map (lambda (bv bb)
		   (scheme-lambda-lift-add-definition
		    `(DEFINE (,bv ,@(append fv* (cadr bb)))
		       ,(caddr bb))))
		 bound-vars bound-bodies)
	    (scheme-lambda-lift
	     (scheme-lambda-lift-vars bound-vars fv* body)
	     vars)))
	 ;;
	 ((equal? tag 'LAMBDA)
	  (let* ((bound-vars (car args))
		 (vars (append bound-vars vars))
		 (body (cadr args)))
	    `(LAMBDA ,bound-vars ,(scheme-lambda-lift body vars))))
	 (else
	  (cons tag
		(map (lambda (e) (scheme-lambda-lift e vars)) args)))))))

(define (scheme-lambda-lift-vars bound-vars fv* e)
  (let loop ((e e))
    (if (not (pair? e))
	e
	(let ((tag (car e))
	      (args (cdr e)))
	  (cond
	   ((member tag bound-vars)
	    (append (cons tag fv*) (map loop args)))
	   ((member tag '(LET LET* LETREC))
	    (let ((headers (car args))
		  (body (cadr args)))
	      `(,tag ,(map (lambda (h) `(,(car h) ,(loop (cadr h))))
			   headers)
		     ,(loop body))))
	   (else
	    (cons tag (map loop args))))))))


;;; compute the intersection of the free variables in `e' and `vars'
(define (scheme-freevars e vars)
  (if (not (pair? e))
      (if (and (symbol? e)
	       (member e vars))
	  (list e)
	  '())
      (let ((tag (car e))
	    (args (cdr e)))
	(cond
	 ((member tag vars)
	  (apply set-union*
		 (cons (list tag)
		       (map (lambda (e) (scheme-freevars e vars)) (cdr
								   e)))))
	 ((pair? tag)
	  (apply set-union*
		 (cons
		  (scheme-freevars tag vars)
		  (map (lambda (e) (scheme-freevars e vars)) (cdr e)))))
	 ((equal? tag 'QUOTE)
	  '())
	 ((equal? tag 'LET)
	  (let* ((headers (car args))
		 (body (cadr args))
		 (bound-vars (map car headers))
		 (es (map cadr headers)))
	    (apply set-union*
		    (cons
		     (set-difference
		      (scheme-freevars body (append bound-vars vars))
		      bound-vars)
		     (map (lambda (e) (scheme-freevars e vars)) es)))))
	 ((equal? tag 'LET*)
	  (let loop ((headers (car args))
		     (bound-vars '())
		     (vars vars))
	    (if (null? headers)
		(set-difference
		 (scheme-freevars (cadr args) vars)
		 bound-vars)
		(let* ((header (car headers))
		       (bound-var (car header)))
		  (set-union (scheme-freevars (cadr header) vars)
			     (set-subtract
			      (loop (cdr headers)
				    (cons bound-var bound-vars)
				    (cons bound-var vars))
			      bound-var))))))
	 ((equal? tag 'LETREC)
	  (let* ((headers (car args))
		 (body (cadr args))
		 (bound-vars (map car headers))
		 (vars (append bound-vars vars)))
	    (set-difference
	    (apply set-union*
		   (cons
		    (scheme-freevars body vars)
		    (map (lambda (h) (scheme-freevars (cadr h) vars))
			 headers)))
	    bound-vars)))
	 ((equal? tag 'LAMBDA)
	  (let* ((bound-vars (car args))
		 (vars (append bound-vars vars))
		 (body (cadr args))) 
	    (set-difference (scheme-freevars body vars)
			    bound-vars)))
	 (else
	  (apply set-union*
		 (map (lambda (e) (scheme-freevars e vars)) args)))))))

;;; process (defdata t (c1 s11 ... s1n) ... (cm sm1 ... smn))
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
				   (list type-name np nc nt) ctor)
				  (loop (cdr ctors) (+ np nc)))))))) dc*)))
(define (scheme->abssyn-one-ctor desc ctor)
  (let* ((the-ctor (car ctor))
	 (the-test (string->symbol
		    (string-append (symbol->string the-ctor) "?")))
	 (selectors (cdr ctor)))
  (cons
   (list the-ctor (annMakeCtor1 desc) (length selectors))
   (cons
    (list the-test (annMakeTest1 the-ctor desc) 1)
    (let loop ((selectors selectors) (i 1))
      (if (null? selectors)
	  '()
	  (cons
	   (list (car selectors) (annMakeSel1 the-ctor desc i) 1)
	   (loop (cdr selectors) (+ i 1))))))))) 

