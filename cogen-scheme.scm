;;; cogen-scheme

;;; copyright 1996, 1997, 1998 by Peter Thiemann
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
;;; (define *scheme->abssyn-label-counter* 0)
(define (scheme->abssyn-make-label)
  (set-scheme->abssyn-label-counter!
	(+ 1 *scheme->abssyn-label-counter*))
  *scheme->abssyn-label-counter*)
;;; (define *scheme->abssyn-static-references* #f)
(define (scheme->abssyn-static-references-yes!)
  (set-scheme->abssyn-static-references! #t))
(define *scheme->abssyn-mutable-variables* '())
(define (scheme->abssyn-mutable-variable! v)
  (set! *scheme->abssyn-mutable-variables*
	(set-include *scheme->abssyn-mutable-variables* v)))
(define (mutable-definition? d)
  (let ((template (cadr d)))
    (and (not (pair? template))
	 (memq template *scheme->abssyn-mutable-variables*))))
(define (scheme->abssyn-d d* def-syntax* ctor-symtab)
  ;; (display-line "scheme->abssyn " ctor-symtab)
  (set-scheme->abssyn-label-counter! 1)
  (set! *scheme->abssyn-mutable-variables* '())
  (set-scheme->abssyn-static-references! #f)
  (let* ((d* (scheme-rename-variables-d d* def-syntax*))
	 ;; (dummy (writelpp d* "/tmp/def1.scm"))
	 (imp-defined-names* (map cadr (filter mutable-definition? d*)))
	 (dummy (set! *scheme->abssyn-mutable-variables*
		      (set-difference *scheme->abssyn-mutable-variables*
				      imp-defined-names*)))
	 (d* (map scheme-wrap-one-d d*))
	 ;; (dummy (writelpp d* "/tmp/def2.scm"))
	 (d* (scheme-lambda-lift-d d*))
	 ;; (dummy (writelpp d* "/tmp/def3.scm"))
	 (symtab
	  (append
	   (map (lambda (d)
		  (let ((template (cadr d)))
		    (if (pair? template)
			(list (car template) scheme->abssyn-make-call (length (cdr template)))
			(list template scheme->abssyn-make-call -1))))
		d*)
	   (list (list 'EVAL annMakeEval 2)
		 (list 'MAKE-CELL (lambda (tag args)
				    (scheme->abssyn-static-references-yes!)
				    (annMakeRef
				     (scheme->abssyn-make-label)
				     (ann-maybe-coerce (car args)))) 1)
		 (list 'CELL-REF (lambda (tag args)
				   (scheme->abssyn-static-references-yes!)
				   (annMakeDeref
				    (ann-maybe-coerce (car args)))) 1)
		 (list 'CELL-SET! (lambda (tag args)
				    (scheme->abssyn-static-references-yes!)
				    (annMakeAssign
				     (scheme->abssyn-make-label)
				     (ann-maybe-coerce (car args))
				     (ann-maybe-coerce (cadr args)))) 2)
		 (list 'CELL-EQ? (lambda (tag args)
				   (scheme->abssyn-static-references-yes!)
				   (annMakeCellEq args)) 2))
	   ctor-symtab)))
    (map (lambda (d) (scheme->abssyn-one-d imp-defined-names* symtab d)) d*))) 

(define (scheme->abssyn-make-call fname args)
  (annMakeCall fname (map ann-maybe-coerce args)))

(define (scheme->abssyn-make-app f args)
  (annMakeApp f (map ann-maybe-coerce args)))

(define (scheme->abssyn-make-ctor1 desc)
  (lambda (ctor args)
    (let ((label (scheme->abssyn-make-label))
	  (args (map ann-maybe-coerce args)))
      (if *scheme->abssyn-let-insertion*
	  (let loop ((args args) (vars '()))
	    (if (null? args)
		(annMakeCtor ctor label desc (map annMakeVar (reverse vars)))
		(let ((newvar (gensym 'ctorarg)))
		  (annMakeLet newvar
			      (car args)
			      (loop (cdr args) (cons newvar vars))))))
	  (annMakeCtor ctor label desc args)))))

(define (scheme->abssyn-one-d imp-defined-names* symtab d)
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
		       (ann-maybe-coerce 
			(scheme->abssyn-e (caddr d) symtab)))))
	  (make-def procname formals body))
	((if (memq template imp-defined-names*)
	     annMakeDefMutable
	     make-def)
	 template #f (scheme->abssyn-e (caddr d) symtab)))))

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
     ((pair? (car e))
      (scheme->abssyn-make-app (loop (car e)) (map loop (cdr e))))
     ((not (symbol? (car e)))
      (error "Illegal expression" e))
     (else
      (let* ((tag (car e))
	     (args (cdr e))
	     (mkproc (assoc tag symtab)))
	(if mkproc
	    (apply (cadr mkproc)
		   (list tag (map loop args)))
	    (cond
	     ((eq? tag 'QUOTE)
	      (annMakeConst (car args)))
	     ((equal? tag 'IF)
	      (let* ((opt-else-branch (cddr args))
		     (else-branch (if (null? opt-else-branch) `'IF-2 (car opt-else-branch))))
		(annMakeCond (loop (car args))
			     (ann-maybe-coerce (loop (cadr args)))
			     (ann-maybe-coerce (loop else-branch)))))
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
	      (if (null? args)
		  (annMakeConst 'begin-0)
		  (scheme->abssyn-e
		   (let loop ((args args))
		     (if (= 1 (length args))
			 (car args)
			 `(LET ((,(gensym 'BEGIN) ,(car args)))
			    ,(loop (cdr args)))))
		   symtab)))
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
		  (ann-maybe-coerce
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
	     (else
	      (annMakeOp tag (map (lambda (arg) (ann-maybe-coerce (loop arg))) args))))))))))

;;; wrap the formal parameters in let-binders around a procedure body 
(define (scheme->abssyn-wrap-in-let formals body)
  (if (not *scheme->abssyn-let-insertion*)
      body
      (let loop ((formals formals))
	(if (null? formals)
	    body
	    (let ((v (car formals)))
	      (annMakeLet v (annMakeVar v) (loop (cdr formals))))))))

(define (scheme-make-var-app var rands)
  (scheme->abssyn-make-app (annMakeVar var) rands))

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

(define-record macro-binding (transformer env))

(define (scheme-rename-variables-d d* def-syntax*)
  (let ((macro-symtab (extend-env* (map cadr def-syntax*)
				   (map (lambda (def-syntax)
					  (make-macro-binding
					   (parse-syntax-rules (caddr def-syntax))
					   the-empty-env))
					def-syntax*)
				   the-empty-env)))
    (for-each-env! (lambda (entry)
		     (macro-binding->env! entry macro-symtab))
		   macro-symtab)
    (set! *scheme-rename-counter* '())
    (map (lambda (d)
	   ;;(display "scheme-rename-variables: ") (display (caadr d)) (newline)
	   (let* ((definer (car d))
		  (template (cadr d))
		  (is-proc-def (pair? template))
		  (fname (if is-proc-def (car template) template))
		  (formals (if is-proc-def (cdr template) '()))
		  (new-formals (map scheme-rename-clone formals))
		  (symtab* (list (extend-env* formals new-formals macro-symtab)))
		  (body-list (cddr d))
		  (body (scheme-body-list->body body-list symtab*))
		  (new-body (scheme-rename-variables symtab* body))
		  (new-template (if is-proc-def (cons fname new-formals) fname)))
	     `(,definer ,new-template ,new-body)))
	 d*)))

(define (scheme-lookup-tag symtab* tag)
  (let loop ((symtab* symtab*) (tag tag))
    (cond
     ((syntax-pop-mark? tag)
      (loop (cdr symtab*) (syntax-marked-exp tag)))
     ((symbol? tag)
      (apply-env (car symtab*) tag (lambda () #f)))
     (else
      #f))))

(define (scheme-rename-variables symtab* e)
  ;; (display-line (spaces (length symtab*)) "scheme-rename-variables: " (length symtab*) " " (syntax-depth e))
  (let loop ((e e))
    (cond
     ((syntax-pop-mark? e)
      (scheme-rename-variables (cdr symtab*) (syntax-marked-exp e)))
     ((symbol? e)
      (let ((found (apply-env (car symtab*) e (lambda () #f))))
	;;(display-line "symbol-e= " e " found= "
	;; (map (lambda (symtab) (apply-env symtab e (lambda () #f))) symtab*))
	(if found
	    (cond
	     ((symbol? found)
	      found)
	     ((macro-binding? found)
	      (error "macro binding used as variable"))
	     (else
	      (loop found)))
	    e)))
     ((not (pair? e))			;some unquoted literal
      e)
     (else				;at this point all marks are stripped? no...
      (let* ((tag (syntax-car e))
	     (args (syntax-cdr e))
	     (found (scheme-lookup-tag symtab* tag)))
	(cond
	 ((macro-binding? found)
	  (let* ((new-symtab* (cons (macro-binding->env found) symtab*))
		 (expansion ((macro-binding->transformer found) (syntax-make-pop-mark e)
								new-symtab*)))
	    (scheme-rename-variables new-symtab* expansion))) ;in the current environment
	 (found
	  (if (and (pair? found) (eq? (car found) 'lambda))
	      (cons (caaddr found) (syntax-map loop args))
	      (cons found (syntax-map loop args))))
	 ;;; need to strip the tag here
	 ((syntax-pair? tag)
	  (let ((new-tag (loop tag))
		(new-args (syntax-map loop args)))
	    (cons new-tag new-args)))
	 ((eq? tag 'QUOTE)
	  ;;(display "!!!Q1 ") (display e) (newline)
	  (syntax-strip-recursively e))
	 ((eq? tag 'BACKQUOTE)
	  (loop (backquote-expander 0 e)))
	 ;; named let
	 ((and (eq? tag 'LET)
	       (not (syntax-pair? (syntax-car args)))
	       (not (syntax-null? (syntax-car args))))
	  ;;(display-line "named-let: " args)
	  (letrec ((list-last (lambda (l) (if (and (pair? l) (null? (cdr l)))
					      (car l)
					      (list-last (cdr l))))))
	    (scheme-rename-variables
	     (cons (list-last symtab*) symtab*)
	     (or ((syntax-rules-transformer '()
					    '(((let name ((v e) ...) body ...)
					       (letrec ((name (lambda (v ...) body ...)))
						 (name e ...))))
					    (lambda () #f))
		  (syntax-make-pop-mark (cons tag args))
		  symtab*)
		 (error "syntax error in named let" e)))))
	 ;; !!! need to strip off SCHEME-POP-MARK
	 ((eq? tag 'LET)
	  ;;(display-line "let: " args)
	  (let* ((bindings (syntax-car args))
		 (formals (syntax-map (lambda (binding) (syntax-strip (syntax-car binding)))
				      bindings))
		 (depths (syntax-map (lambda (binding)
				       (syntax-depth (syntax-car binding))) bindings))
		 (bodies (syntax-map (lambda (x) (syntax-car (syntax-cdr x))) bindings))
		 (body-list (syntax-cdr args))
		 (new-formals (map scheme-rename-clone formals))
		 (new-bodies (map loop bodies))
		 (new-symtab*
		  (let rec ((formals formals)
			    (new-formals new-formals)
			    (depths depths)
			    (symtab* symtab*))
		    (if (null? formals)
			symtab*
			(rec (cdr formals)
			     (cdr new-formals)
			     (cdr depths)
			     (scheme-extend-symtab* (list (car formals))
						    (list (car new-formals))
						    symtab*
						    (car depths))))))
		 (new-body (scheme-rename-variables
			    new-symtab*
			    (scheme-body-list->body body-list new-symtab*))))
	    (let loop ((new-formals new-formals)
		       (new-bodies new-bodies))
	      (if (null? new-formals)
		  new-body
		  `(LET ((,(car new-formals) ,(car new-bodies)))
		     ,(loop (cdr new-formals) (cdr
					       new-bodies)))))))
	 ;;
	 ((eq? tag 'LETREC)
	  (let* ((bindings (syntax-car args))
		 (formals (syntax-map (lambda (binding) (syntax-strip (syntax-car binding)))
				      bindings))
		 (depths (syntax-map (lambda (binding)
				       (syntax-depth (syntax-car binding))) bindings))
		 (bodies (syntax-map (lambda (x) (syntax-car (syntax-cdr x))) bindings))
		 (body-list (syntax-cdr args))
		 (new-formals (map scheme-rename-clone formals))
		 ;; assume [no longer that] all bodies are lambdas
		 (arities (syntax-map (lambda (body)
					(and (syntax-pair? body)
					     (syntax-eq-symbol? 'LAMBDA (syntax-car body) symtab*)
					     (syntax-car (syntax-cdr body))))
				      bodies))
		 (munge (lambda (new-formal arity)
			  (if arity
			      (let ((arity (syntax-strip arity)))
				`(LAMBDA ,arity (,new-formal ,@arity)))
			      new-formal)))
		 (new-symtab*
		  (let rec ((formals formals)
			    (new-formals (map munge new-formals arities))
			    (depths depths)
			    (symtab* symtab*))
		    (if (null? formals)
			symtab*
			(rec (cdr formals)
			     (cdr new-formals)
			     (cdr depths)
			     (scheme-extend-symtab* (list (car formals))
						    (list (car new-formals))
						    symtab*
						    (car depths))))))
		 (new-bodies (map (lambda (formal body)
				    (scheme-rename-variables new-symtab* body))
				  formals bodies))
		 (new-body (scheme-rename-variables
			    new-symtab*
			    (scheme-body-list->body body-list new-symtab*))))
;;;	    (display (list "new-symtab*" depth (map (lambda (symtab)
;;;						      (apply-env symtab 'input (lambda () #f)))
;;;						    new-symtab*)))
;;;	    (newline) (display (list "body-list=" body-list)) (newline)
	    (let loop ((new-formals new-formals)
		       (arities arities)
		       (new-bodies new-bodies)
		       (new-headers '())
		       (final-body new-body))
	      (if (null? new-formals)
		  `(LETREC ,(reverse new-headers) ,final-body)
		  (let ((new-formal (car new-formals))
			(arity (car arities))
			(new-body (car new-bodies)))
		    (if arity
			(loop (cdr new-formals)
			      (cdr arities)
			      (cdr new-bodies)
			      (cons (list new-formal new-body) new-headers)
			      final-body)
			(begin
			  (scheme->abssyn-mutable-variable! new-formal)
			  `(LET ((,new-formal #f))
			     ,(loop (cdr new-formals)
				    (cdr arities)
				    (cdr new-bodies)
				    new-headers
				    `(BEGIN (SET! ,new-formal ,new-body)
					    ,final-body))))))))))
	 ;;
	 ((eq? tag 'LAMBDA)
	  ;;(display-line "lambda: " args)
	  (let* ((depth (syntax-depth args))
		 (formals (syntax-car args))
		 (new-formals (let loop ((formals formals))
				(cond
				 ((syntax-pair? formals)
				  (cons (scheme-rename-clone (syntax-strip (syntax-car formals)))
					(loop (syntax-cdr formals))))
				 ((syntax-null? formals)
				  '())
				 (else
				  (scheme-rename-clone (syntax-strip formals))))))
		 (new-symtab* (scheme-extend-symtab* (scheme-formals->vars formals)
						     (scheme-formals->vars new-formals)
						     symtab*
						     depth))
		 (body-list (syntax-cdr args))
		 (body (scheme-body-list->body body-list new-symtab*))
		 (new-body (scheme-rename-variables new-symtab* body)))
	    `(LAMBDA ,new-formals ,new-body)))
	 ((eq? tag 'SET!)
	  ;;(display-line "set!: " args)
	  (let ((renamed-args (syntax-map loop args)))
	    (scheme->abssyn-mutable-variable! (syntax-strip (syntax-car renamed-args)))
	    `(SET! ,@renamed-args)))
	 ;; local macros
	 ;; (let-syntax ((v syntax-rules)) body)
	 ((eq? tag 'LET-SYNTAX)
	  (if (> (length symtab*) 1)
	      (error "let-syntax generated by macro:" e))
	  (let* ((depth (syntax-depth args))
		 (phrase (syntax-car (syntax-car args)))
		 (formal (syntax-car phrase))
		 (new-symtab* (scheme-extend-symtab*
			       (list formal)
			       (list (make-macro-binding
				      (parse-syntax-rules
				       (syntax-car (syntax-cdr args)))
				      (car symtab*)))
			       symtab*
			       depth))
		 (body-list (syntax-cdr args))
		 (body (scheme-body-list->body body-list new-symtab*))
		 (new-body (scheme-rename-variables new-symtab* body)))
	    new-body))
	 ;;(letrec-syntax ((...)...) body)
	 ((eq? tag 'LETREC-SYNTAX)
	  (if (> (length symtab*) 1)
	      (error "letrec-syntax generated by macro:" e))
	  (let* ((bindings (syntax-car args))
		 (formals (syntax-map
			   (lambda (binding)
			     (syntax-strip (syntax-car binding)))
			   bindings))
		 (depths (syntax-map
			  (lambda (binding)
			    (syntax-depth (syntax-car binding)))
			  bindings))
		 (rules (syntax-map (lambda (x) (syntax-car (syntax-cdr x)))
				    bindings))
		 (new-symtab* (scheme-extend-symtab*
			       formals
			       (map (lambda (rule-set)
				      (make-macro-binding
				       (parse-syntax-rules
					rule-set)
				       #f))
				    rules)
			       symtab*
			       0))	;depth!
		 (fix-symtab* (for-each-env!
			       (lambda (entry)
				 (if (macro-binding? entry)
				     (macro-binding->env! entry
							  (car new-symtab*))))
			       (car new-symtab*)))
		 (body-list (syntax-cdr args))
		 (new-body (scheme-rename-variables
			    new-symtab*
			    (scheme-body-list->body body-list new-symtab*))))
	    new-body))
	 (else
	  ;;(display-line "tag= " tag)
	  (cons (syntax-strip tag) (syntax-map loop args)))))))))

(define (scheme-extend-symtab* key* value* symtab* depth)
  (let loop ((depth depth) (symtab* symtab*))
    (if (zero? depth)
	(cons (extend-env* key*
			   value*
			   (car symtab*))
	      (cdr symtab*))
	(cons (car symtab*)
	      (loop (- depth 1) (cdr symtab*))))))

(define (scheme-formals->vars formals)
  (let loop ((formals formals) (acc '()))
    (cond
     ((syntax-pair? formals)
      (loop (syntax-cdr formals) (cons (syntax-strip (syntax-car formals)) acc)))
     ((syntax-null? formals)
      acc)
     (else
      (cons (syntax-strip formals) acc)))))

;;; attention: inner definitions must have form (define (P V*) D0* E) 
(define (scheme-body-list->body body-list symtab*)
  ;;(display (list "body-list:" body-list)) (newline)
  (let* ((depth (syntax-depth body-list))
	 (wrapper (lambda (x) x)))	;should really refer to the standard env
    (if (= 1 (length (syntax-strip body-list)))
	(syntax-car body-list)
	(let loop ((body-list body-list) (definitions '()))
	  (if (and (syntax-pair? body-list)
		   (syntax-pair? (syntax-car body-list))
		   (syntax-eq-symbol? 'DEFINE
				      (syntax-car (syntax-car body-list))
				      symtab*))
	      (loop (syntax-cdr body-list)
		    (cons (syntax-car body-list) definitions))
	      (let ((real-body (if (= 1 (length (syntax-strip body-list)))
				   (syntax-car body-list)
				   `(,(wrapper 'BEGIN)
				     ,@(syntax-map (lambda (x) x) body-list))))
		    (def->letrec-clause
		      (lambda (def)
			(let ((template (syntax-car (syntax-cdr def)))
			      (inner-body-list (syntax-cdr (syntax-cdr def))))
			  (if (syntax-pair? template)
			      `(,(syntax-car template)
				(,(wrapper 'LAMBDA)
				 ,(syntax-cdr template)
				 ,@(syntax-map (lambda (x) x) inner-body-list)))
			      `(,template ,(syntax-car inner-body-list)))))))
		(if (null? definitions)
		    real-body
		    `(,(wrapper 'LETREC)
		      ,(map def->letrec-clause definitions)
		      ,real-body))))))))

(define (backquote-expander level e)
  (cond
   ((pair? e)
    (let ((tag (car e)))
      (case tag
	((QUASIQUOTE)
	 (let ((qq 'quasiquote))
	   `(cons ,qq
		  ,(backquote-expander (+ level 1) (cadr e)))))
	((UNQUOTE)
	 (if (zero? level)
	     (cadr e)
	     (let ((uq 'unquote))
	       `(list ,uq
		      ,(backquote-expander (- level 1) (cadr e))))))
	((UNQUOTE-SPLICING)
	 (if (zero? level)
	     (error "UNQUOTE-SPLICING on wrong level")
	     (let ((uqs 'unquote-splicing))
	       `(list ,uqs
		      ,(backquote-expander (- level 1) (cadr e))))))
	(else
	 (if (and (pair? tag)
		  (eq? (car tag) 'UNQUOTE-SPLICING)
		  (zero? level))
	     `(append ,(cadr tag) ,(backquote-expander level (cdr e)))
	     `(cons ,(backquote-expander level tag)
		    ,(backquote-expander level (cdr e))))))))
   ((or (symbol? e) (null? e))
    `(quote ,e))
   ((vector? e)
    (list->vector (backquote-expander level (vector->list e))))
   (else
    e)))

;;; before second step:
;;; wrap mutable variables into explicit boxes

(define (scheme-wrap-mutables new->old body)
  (let loop ((new->old new->old))
    (if (null? new->old)
	body
	`(LET ((,(cdar new->old) (MAKE-CELL ,(caar new->old))))
	   ,(loop (cdr new->old))))))

(define (scheme-wrap-binding bound-vars body build)
  (let loop ((old-vars bound-vars) (new-vars '()) (new->old '()))
    (if (null? old-vars)
	(build (reverse new-vars)
	       (scheme-wrap-mutables new->old body))
	(if (pair? old-vars)
	    (let ((old-var (car old-vars))
		  (old-vars (cdr old-vars)))
	      (if (member old-var *scheme->abssyn-mutable-variables*)
		  (let ((new-var (gensym old-var)))
		    (loop old-vars
			  (cons new-var new-vars)
			  (cons (cons new-var old-var) new->old)))
		  (loop old-vars (cons old-var new-vars) new->old)))
	    (let ((old-var old-vars))
	      (if (member old-var *scheme->abssyn-mutable-variables*)
		  (let ((new-var (gensym old-var)))
		    (build (append (reverse new-vars) new-var)
			   (scheme-wrap-mutables (cons (cons new-var old-var) new->old)
						 body)))
		  (build (append (reverse new-vars) old-var)
			 (scheme-wrap-mutables new->old body))))))))

(define (scheme-wrap-one-d d)
  (let ((definer (car d))
	(template (cadr d))
	(wrapped-body (scheme-wrap-e (caddr d))))
    (if (pair? template)
	(scheme-wrap-binding (cdr template)
			     wrapped-body
			     (lambda (new-vars body)
			       `(,definer (,(car template) ,@new-vars)
					  ,body)))
	`(,definer ,template ,wrapped-body))))

(define (scheme-wrap-e e)
  (if (not (pair? e))
      (if (member e *scheme->abssyn-mutable-variables*)
	  `(CELL-REF ,e)
	  e)
      (let ((tag (car e))
	    (args (cdr e)))
	(cond
	 ((pair? tag)
	  (let ((rator (scheme-wrap-e tag))
		(rands (map scheme-wrap-e args)))
	    `(,rator ,@rands)))
	 ((equal? tag 'QUOTE)
	  ;;(display "!!!Q2 ") (display e) (newline)
	  e)
	 ((equal? tag 'SET!)
	  (let ((var (car args))
		(exp (scheme-wrap-e (cadr args))))
	    (if (memq var *scheme->abssyn-mutable-variables*)
		`(CELL-SET! ,var ,exp)
		`(SET! ,var ,exp))))
	 ;; only one binder in a LET
	 ((equal? tag 'LET)
	  (let* ((header (caar args))
		 (body (cadr args))
		 (bound-var (car header))
		 (bound-body (cadr header))
		 (wrapper (if (member bound-var *scheme->abssyn-mutable-variables*)
			      (lambda (body) `(MAKE-CELL ,body))
			      (lambda (body) body))))
	    `(LET ((,bound-var
		    ,(wrapper (scheme-wrap-e bound-body))))
	       ,(scheme-wrap-e body))))
	 ;;
	 ((equal? tag 'LETREC)
	  (let* ((headers (car args))
		 (body (cadr args))		 
		 (wrapper (lambda (bound-var)
			    (if (member bound-var *scheme->abssyn-mutable-variables*)
				(lambda (body) `(MAKE-CELL ,body))
				(lambda (body) body)))))
	    `(LETREC ,(map (lambda (header)
			     (let ((bound-var (car header)))
			       (cons bound-var
				     ((wrapper bound-var) (scheme-wrap-e (cdr header))))))
			   headers)
	       ,(scheme-wrap-e body))))
	 ;;
	 ((equal? tag 'LAMBDA)
	  (let* ((bound-vars (car args))
		 (wrapped-body (scheme-wrap-e (cadr args))))
	    (scheme-wrap-binding bound-vars
				 wrapped-body
				 (lambda (new-vars body)
				   `(LAMBDA (,@new-vars) ,body)))))
	 (else
	  (cons tag (map scheme-wrap-e args)))))))

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
	  ;;(display "!!!Q3 ") (display e) (newline)
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
	    `(LAMBDA ,bound-vars
	       ,(scheme-lambda-lift body vars definer))))
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
	   ((eq? 'QUOTE tag)
	    e)
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


