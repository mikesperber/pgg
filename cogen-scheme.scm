;;; cogen-scheme

;;; copyright 1996 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; convert side-effect free scheme to abstract syntax
;;; + translate and, or to if
;;; + simplify let to (let ((V E)) E) 
;;; + eta-expand procedures and
;;; - operators which occur as variables
;;; + add let binders for every parameter of a procedure
;;; + add letrec and lambda lifting
;;; + handle begin
;;; + nested define
;;; + treat eval special

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
	 ;; (dummy (p d*))
	 (d* (scheme-lambda-lift-d d*))
	 ;; (dummy (p d*))
	 (symtab
	  (cons
	   (list 'EVAL annMakeEval 2)
	   (append
	    (map (lambda (d)
		   (let ((template (cadr d)))
		     (if (pair? template)
			 (list (car template) annMakeCall (length (cdr template)))
			 (list template annMakeCall -1))))
		 d*)
	    ctor-symtab))))
    (map (lambda (d) (scheme->abssyn-one-d symtab d)) d*))) 

(define (scheme->abssyn-one-d symtab d)
  (let ((make-def (case (car d)
		    ((define) annMakeDef)
		    ((define-without-memoization) annMakeDefWithoutMemoization)))
	(template (cadr d)))
    (if (pair? template)
	(let* ((procname (car template))
	       (formals (cdr template))
	       (symtab (append
			(map (lambda (v)
			       (list v scheme-make-var-app -1)) formals)
			symtab))
	       (body (scheme->abssyn-wrap-in-let
		       formals
		       (scheme->abssyn-maybe-coerce 
			(scheme->abssyn-e (caddr d) symtab)))))
	  (make-def procname formals body))
	(make-def template #f (scheme->abssyn-e (caddr d) symtab)))))

(define (scheme->abssyn-maybe-coerce e)
  (annMakeOp INTERNAL-IDENTITY (list e)))
(define (scheme->abssyn-e e symtab)
  (let loop ((e e))
    (cond
     ((symbol? e)
      (let ((syminfo (assoc e symtab)))
	(if (and syminfo (>= (caddr syminfo) 0))
	    (let* ((arity (caddr syminfo))
		   (newvars (nlist arity (lambda () (gensym 'XXX)))))
	      (loop `(LAMBDA (,@newvars) (,e ,@newvars))))
	    (scheme->abssyn-maybe-coerce (annMakeVar e)))))
     ((not (pair? e))
      (annMakeConst e))
     ((equal? (car e) 'QUOTE)
      (annMakeConst (cadr e)))
     (else
      (let* ((tag (car e))
	     (args (cdr e))
	     (mkproc (assoc tag symtab)))
	(if mkproc
	    (apply (cadr mkproc)
		   (list tag (map scheme->abssyn-maybe-coerce (map loop args))))
	    (cond
	     ((equal? tag 'IF)
	      (annMakeCond (loop (car args))
			   (scheme->abssyn-maybe-coerce (loop (cadr args)))
			   (scheme->abssyn-maybe-coerce (loop (caddr args)))))
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
	     ((and (equal? tag 'LAMBDA)
		   (list? (car args)))
	      (let* ((formals (car args))
		     (symtab (append
			      (map (lambda (var) (list var scheme-make-var-app -1))
				   formals)
			      symtab)))
		(annMakeLambda
		 (scheme->abssyn-make-label)
		 (car args)
		 (scheme->abssyn-wrap-in-let
		  formals
		  (scheme->abssyn-maybe-coerce
		   (scheme->abssyn-e (cadr args)
				     symtab))))))
	     ((equal? tag 'LAMBDA)
	      (let loop ((fixed-formals '())
			 (rest (car args)))
		(if (pair? rest)
		    (loop (cons (car rest) fixed-formals) (cdr rest))
		    (let* ((fixed-formals (reverse fixed-formals))
			   (var-formal rest)
			   (formals (cons var-formal fixed-formals))
			   (symtab (append
				    (list (list var-formal scheme-make-var-app -1))
				    (map (lambda (var) (list var scheme-make-var-app -1))
					 formals)
				    symtab)))
		      (annMakeVLambda
		       (scheme->abssyn-make-label)
		       fixed-formals
		       var-formal
		       (scheme->abssyn-wrap-in-let formals
						   (scheme->abssyn-e (cadr args)
								     symtab)))))))
	     ((pair? tag)
	      (annMakeApp (loop tag) (map scheme->abssyn-maybe-coerce (map loop args))))
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
  (let ((symtab (map (lambda (d)
		       (let ((template (cadr d)))
			 (cons (car template) `(LAMBDA ,(cdr template) ,template))))
		     d*)))
    (map (lambda (d)
	   ;;(display "scheme-rename-variables: ") (display (caadr d)) (newline)
	   (let* ((definer (car d))
		  (template (cadr d))
		  (is-proc-def (pair? template))
		  (fname (if is-proc-def (car template) template))
		  (formals (if is-proc-def (cdr template) '()))
		  (body-list (cddr d))
		  (body (scheme-body-list->body body-list))
		  (new-formals (map scheme-rename-clone formals))
		  (new-symtab (append (map cons formals new-formals) symtab))
		  (new-body (scheme-rename-variables new-symtab body))
		  (new-template (if is-proc-def (cons fname new-formals) fname)))
	   `(,definer ,new-template ,new-body))) d*)))

(define (scheme-rename-variables symtab e)
  (if (symbol? e)
      (let ((found (assoc e symtab)))
	(if found
	    (cdr found)
	    e))
      (if (not (pair? e))
	  e
	  (let ((tag (car e))
		(args (cdr e)))
	    (cond
	     ((pair? tag)
	      (let ((new-tag (scheme-rename-variables symtab tag))
		    (new-args (map (lambda (e) (scheme-rename-variables
						symtab e)) args)))
		(cons new-tag new-args)))
	     ((equal? tag 'QUOTE)
	      e)
	     ;;
	     ((equal? tag 'CASE)
	      (scheme-rename-variables
	       symtab
	       (let ((var (gensym 'CASE))
		     (key (car args))
		     (clauses (cdr args)))
	       `(LET ((,var ,key))
		  (COND
		   ,@(let loop ((clauses clauses))
		       (if (null? clauses)
			   '()
			   (let ((clause (car clauses)))
			     (if (equal? (car clause) 'ELSE)
				 (list clause)
				 (let ((data (car clause))
				       (commands (cdr clause)))
				   (cons
				    `((MEMV ,var ',data)
				      ,@commands)
				    (loop (cdr clauses)))))))))))))
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
		     (new-symtab (append (map cons formals new-formals) symtab))
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
		     ;; assume all bodies are lambdas
		     (arities (map (lambda (body) (cadr body)) bodies))
		     (munge (lambda (formal new-formal arity)
			      (cons formal `(LAMBDA ,arity (,new-formal ,@arity)))))
		     (new-symtab (append (map munge formals new-formals arities) symtab))
		     (new-bodies (map (lambda (formal body)
					;;(display "letrec: ") (display formal) (newline)
					(scheme-rename-variables new-symtab body))
				      formals bodies))
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
		     (new-formals (let loop ((formals formals))
				    (cond
				     ((pair? formals)
				      (cons (scheme-rename-clone (car formals))
					    (loop (cdr formals))))
				     ((null? formals)
				      '())
				     (else
				      (scheme-rename-clone formals)))))
		     (new-symtab (append (map cons
					      (scheme-formals->vars formals)
					      (scheme-formals->vars new-formals))
					 symtab))
		     (new-body (scheme-rename-variables new-symtab body)))
		`(LAMBDA ,new-formals ,new-body)))
	     (else
	      (let* ((found (assoc tag symtab))
		     (new-tag (if found
				  (if (symbol? (cdr found))
				      (cdr found)
				      (caaddr (cdr found)))
				  tag)))
		(cons new-tag (map (lambda (e) (scheme-rename-variables
						symtab e)) args)))))))))

(define (scheme-formals->vars formals)
  (let loop ((formals formals) (acc '()))
    (cond
     ((pair? formals)
      (loop (cdr formals) (cons (car formals) acc)))
     ((null? formals)
      acc)
     (else
      (cons formals acc)))))

;;; attention: inner definitions must have form (define (P V*) D0* E) 
(define (scheme-body-list->body body-list)
  ;;(display (list "body-list:" body-list)) (newline)
  (if (= 1 (length body-list))
      (car body-list)
      (let loop ((body-list body-list) (definitions '()))
	(if (and (pair? body-list)
		 (pair? (car body-list))
		 (equal? 'DEFINE (caar body-list)))
	    (loop (cdr body-list) (cons (car body-list) definitions))
	    (let ((real-body (if (= 1 (length body-list))
				 (car body-list)
				 `(BEGIN ,@body-list))))
	      (if (null? definitions)
		  real-body
		  `(LETREC
		       ,(map (lambda (d)
			       `(,(caadr d)
				 (LAMBDA ,(cdadr d) ,@(cddr d))))
			     definitions)
		     ,real-body)))))))

;;; second step: perform lambda lifting for LETREC forms, assumes
;;; first step has been performed
(define *scheme-lambda-lift-definitions* '())
(define (scheme-lambda-lift-add-definition d)
  (set! *scheme-lambda-lift-definitions*
	(cons d *scheme-lambda-lift-definitions*)))
(define (scheme-lambda-lift-d d*)
  (set! *scheme-lambda-lift-definitions* '())
  (let ((old-d* (map scheme-lambda-lift-one-d d*)))
    (append old-d* *scheme-lambda-lift-definitions*)))

(define (scheme-lambda-lift-one-d d)
  (let ((definer (car d))
	(template (cadr d)))
  `(,definer ,template
     ,(scheme-lambda-lift (caddr d)
			  (if (pair? template) (cdr template) '())
			  definer))))

;;; scheme-lambda-lift lifts all LETREC expressions
(define (scheme-lambda-lift e vars definer)
  (if (not (pair? e))
      e
      (let ((tag (car e))
	    (args (cdr e)))
	(cond
	 ((pair? tag)
	  (let ((rator (scheme-lambda-lift tag vars definer))
		(rands (map (lambda (e)
			      (scheme-lambda-lift e vars definer))
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
		    ,(scheme-lambda-lift bound-body vars definer)))
	       ,(scheme-lambda-lift body (cons bound-var vars)
				    definer))))
	 ;;
	 ((equal? tag 'LETREC)
	  (let* ((headers (car args))
		 (body (cadr args))
		 (bound-vars (map car headers))
		 (dependencies (map (lambda (h)
				      (scheme-freevars (cadr h) bound-vars))
				    headers))
		 (call-graph (map cons bound-vars dependencies))
		 (free-vars (map (lambda (h)
				   (scheme-freevars (cadr h) vars))
				 headers))
		 (b/free-vars (map cons bound-vars free-vars))
		 (b/free-vars (scheme-lambda-lift-fix call-graph b/free-vars))
		 ;;(new-vars (append bound-vars vars))
		 ;;(fv* (apply set-union* free-vars))
		 (bound-bodies
		  (map (lambda (h)
			 (scheme-lambda-lift
			  (scheme-lambda-lift-vars b/free-vars
						   (cadr h))
			  vars
			  definer))
		       headers)))
	    (map (lambda (b/fv* bb)
		   (scheme-lambda-lift-add-definition
		    `(,definer (,@b/fv* ,@(cadr bb))
		       ,(caddr bb))))
		 b/free-vars bound-bodies)
	    (scheme-lambda-lift
	     (scheme-lambda-lift-vars b/free-vars body)
	     vars
	     definer)))
	 ;;
	 ((equal? tag 'LAMBDA)
	  (let* ((bound-vars (car args))
		 (vars (append (scheme-formals->vars bound-vars) vars))
		 (body (cadr args)))
	    `(LAMBDA ,bound-vars ,(scheme-lambda-lift body vars definer))))
	 (else
	  (cons tag
		(map (lambda (e) (scheme-lambda-lift e vars definer)) args)))))))

(define (transpose-graph graph)
  (let ((skeleton (map (lambda (p) (list (car p))) graph)))
    (let loop ((graph graph))
      (if (null? graph)
	  skeleton
	  (let* ((adj (car graph))
		 (source (car adj))
		 (targets (cdr adj)))
	    (for-each (lambda (p)
			(if (member (car p) targets)
			    (set-cdr! p (set-union (list source) (cdr p)))))
		      skeleton)
	    (loop (cdr graph)))))))

(define (scheme-lambda-lift-step call-graph b/free-vars)
  (let ((get-free (lambda (bv) (cdr (assoc bv b/free-vars)))))
    (map (lambda (p)
	   (cons (car p) (apply set-union* (map get-free p))))
	 call-graph)))

(define (scheme-lambda-lift-fix call-graph b/free-vars)
  (let loop ((b/free-vars b/free-vars))
    (let ((b/f-new (scheme-lambda-lift-step call-graph
					    b/free-vars)))
      (if (and-map2 (lambda (b/f1 b/f2)
		      (set-equal? (cdr b/f1) (cdr b/f2)))
		    b/free-vars b/f-new)
	  b/free-vars
	  (loop b/f-new)))))

(define (scheme-lambda-lift-vars b/free-vars e)
  (let loop ((e e))
    (if (not (pair? e))
	e
	(let* ((tag (car e))
	       (args (cdr e))
	       (b/fv* (assoc tag b/free-vars)))
	  (cond
	   (b/fv*
	    (append b/fv* (map loop args)))
	   ((member tag '(LET LET* LETREC))
	    (let ((headers (car args))
		  (body (cadr args)))
	      `(,tag ,(map (lambda (h) `(,(car h) ,(loop (cadr h))))
			   headers)
		     ,(loop body))))
	   (else
	    (cons (loop tag) (map loop args))))))))


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
	  (let* ((bound-vars (scheme-formals->vars (car args)))
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
;; scheme->abssyn-define-type constructs a symbol table for conversion
;; to abstract syntax out of 
;; dc* - list of (define-data ...) forms
;; dt* - list of (define-type ...) forms
(define (scheme->abssyn-define-type dc* dt* do* dm*)
  (append
   (map scheme->abssyn-one-deftype dt*)
   (map scheme->abssyn-one-defop do*)
   (map scheme->abssyn-one-defmemo dm*)
   (apply append (map scheme->abssyn-one-defdata dc*))))
(define (scheme->abssyn-one-defdata dc)
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
		    (loop (cdr ctors) (+ np nc))))))))
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
;;; process (define-type (P B1 ... Bn) B0)
(define (scheme->abssyn-one-deftype dt)
  (let ((template (cadr dt))
	(result-type (caddr dt)))
    (list (car template) annMakeCall (length (cdr template)))))

;;; process (define-primitive ...)
;;; accepts the following syntax for definitions of primitive operators
;;; D  ::= (define-primitive O T [dynamic|error|opaque])
(define (scheme->abssyn-one-defop dt)
  (let* ((op-name (cadr dt))
	 (op-type (caddr dt))
	 (op-optional (cdddr dt))
	 (op-option (and (pair? op-optional) (car op-optional)))
	 (op-apair (assoc op-option bta-property-table)) ;defined in cogen-eq-flow
	 (st-entry (list op-name
			 (annMakeOp1
			  (not (equal? op-option 'opaque))   ;opacity
			  (and op-apair (cdr op-apair))          ;property (a function)
			  #f
			  (parse-type op-type))	          ;type
			 -1
			 ;;(length op-rand-types)
			 )))
    st-entry))

(define (scheme->abssyn-one-defmemo dm)
  (let* ((memo-name (cadr dm))
	 (level (caddr dm)))
    (list memo-name
	  (annMakeOp1 #t
		      (bta-make-memo-property level)
		      (bta-make-memo-postprocessor level)
		      (parse-type '(all t t)))
	  1)))

;;; T  ::= (all TV T) | (rec TV T) | (TC T*) | TV
;;; TV type variable (must be bound by rec or all)
;;; TC type constructor
;;; abstract syntax:
(define-record primop (op-name op-type op-prop))
(define-record primop-tapp (tcon types))
(define-record primop-trec (tvar type))
(define-record primop-tall (tvar type))
(define-record primop-tvar (tvar))

(define parse-type
  (lambda (texp)
    (let loop ((texp texp)
	       (tenv the-empty-env))
      (cond
       ((pair? texp)
	(if (member (car texp) '(all rec))
	    (let ((tvar (cadr texp)))
	      ((if (equal? (car texp) 'rec)
		   make-primop-trec
		   make-primop-tall)
	       tvar
	       (loop (caddr texp)
		     (extend-env tvar (make-primop-tvar tvar) tenv))))
	    (make-primop-tapp (car texp)
			      (map (lambda (texp)
				     (loop texp tenv))
				   (cdr texp)))))
       (else
	(apply-env tenv texp
		   (lambda ()
		     (make-primop-tapp texp '()))))))))
