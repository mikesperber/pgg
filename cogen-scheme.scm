;;; cogen-scheme

;;; copyright © 1996-2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; convert Scheme to abstract syntax
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

(define (scheme-desugar d*)
  (gensym-reset!)
  (set! *scheme->abssyn-mutable-variables* '())
  (scheme-rename-variables-d '() '() d*))

(define (scheme->abssyn-d d* ctor-symtab)
  ;; (display-line "scheme->abssyn " ctor-symtab)
  ;; (gensym-reset!)
  (set-scheme->abssyn-label-counter! 1)
  ;; (set! *scheme->abssyn-mutable-variables* '())
  (set-scheme->abssyn-static-references! #f)
  (let* ((imp-defined-names* (map cadr (filter mutable-definition? d*)))
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
		 (list 'SET! (lambda (tag args)
			       (annMakeFullOp tag #f 'opaque #f #f
					      (list (car args)
						    (ann-maybe-coerce (cadr args))))) 2)
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
		 (list 'MAKE-VECTOR (lambda (tag args)
				      (scheme->abssyn-static-references-yes!)
				      (let ((arg1 (car args))
					    (arg2 (if (null? (cdr args))
						      (annMakeConst #f)
						      (cadr args))))
					(annMakeVector
					 (scheme->abssyn-make-label)
					 (ann-maybe-coerce arg1)
					 (ann-maybe-coerce arg2)))) 2)
		 (list 'VECTOR-REF (lambda (tag args)
				     (scheme->abssyn-static-references-yes!)
				     (annMakeVref
				      (ann-maybe-coerce (car args))
				      (ann-maybe-coerce (cadr args)))) 2)
		 (list 'VECTOR-LENGTH (lambda (tag args)
					(scheme->abssyn-static-references-yes!)
					(annMakeVlen
					 (ann-maybe-coerce (car args)))) 1)
		 (list 'VECTOR-SET! (lambda (tag args)
				      (scheme->abssyn-static-references-yes!)
				      (annMakeVset
				       (scheme->abssyn-make-label)
				       (ann-maybe-coerce (car args))
				       (ann-maybe-coerce (cadr args))
				       (ann-maybe-coerce (caddr args)))) 3)
		 (list 'VECTOR-FILL! (lambda (tag args)
				       (scheme->abssyn-static-references-yes!)
				       (annMakeVfill
					(scheme->abssyn-make-label)
					(ann-maybe-coerce (car args))
					(ann-maybe-coerce (cadr args)))) 2)
		 (list 'CELL-EQ? (lambda (tag args)
				   (scheme->abssyn-static-references-yes!)
				   (annMakeCellEq args)) 2))
	   ctor-symtab)))
    (map (lambda (d) (scheme->abssyn-one-d imp-defined-names* symtab d)) d*))) 

(define (scheme->abssyn-make-call fname args)
  (let ((v (annMakeVar fname)))
    (annSetVarCall! v #t)
    (annMakeApp v (map ann-maybe-coerce args)))  
  ;; (annMakeCall fname (map ann-maybe-coerce args))
  )

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
	     ((eq? tag 'IF)
	      (let* ((opt-else-branch (cddr args))
		     (else-branch (if (null? opt-else-branch) `'IF-2 (car opt-else-branch))))
		(annMakeCond (loop (car args))
			     (ann-maybe-coerce (loop (cadr args)))
			     (ann-maybe-coerce (loop else-branch)))))
	     ((eq? tag 'LET)
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
	     ((eq? tag 'LET*)
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
	     ;; BEGIN has gotten its own identity
	     ((eq? tag 'BEGIN)
	      (if (null? args)
		  (annMakeConst 'begin-0)
		  (let loop ((args args) (l (- (length args) 1)))
		    (if (zero? l)
			(scheme->abssyn-e (car args) symtab)
			(annMakeBegin
			 (scheme->abssyn-e (car args) symtab)
			 (loop (cdr args) (- l 1)))))))
	     ;;
	     ((and (or (eq? tag 'LAMBDA) (eq? tag 'LAMBDA-POLY))
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
				     symtab)))
		 (eq? tag 'LAMBDA-POLY))))
	     ;;
	     ((eq? tag 'LAMBDA)
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
  (let* ((var (syntax-strip var))
	 (var-counter
	  (or (assoc var *scheme-rename-counter*)
	      (begin (set! *scheme-rename-counter*
			   (cons (cons var 0) *scheme-rename-counter*))
		     (car *scheme-rename-counter*)))))
    (set-cdr! var-counter (+ 1 (cdr var-counter)))
    (string->symbol
     (string-append
      (symbol->string var) "_" (number->string (cdr var-counter))))))

(define-record macro-binding (transformer env))
; the env part contains a *reference* to the environment!
(define-record arity-binding (name eta))
; used for procs with known arity, eta is their eta expansion

(define (scheme-rename-variables-d d* def-syntax* not-recognized*)
  (let* ((start (empty-boxed-env))
	 (top-level-box (extend-boxed-env*
			 (map cadr def-syntax*)
			 (map (lambda (def-syntax)
				(make-macro-binding
				 (parse-syntax-rules (caddr def-syntax) start)
				 #f))	;should never be accessed
			      def-syntax*)
			 start)))
    (let loop ((other* not-recognized*)
	       (count (length not-recognized*))
	       (reset (+ (length not-recognized*) 1)))
      (if (pair? other*)
	  (let ((other (car other*))
		(other* (cdr other*)))
	    (if (list? other)
		(let ((tag (car other)))
		  (cond
		   ((apply-boxed-env top-level-box (car other) (lambda () #f))
		    =>
		    (lambda (found)
		      (let ((expansion
			     ((macro-binding->transformer found)
			      other top-level-box))) ;!!! PJT: CHANGED !!!
			(loop (cons expansion other*) reset reset))))
		   (else
		    (case tag
		      ((define define-without-memoization)
		       (set! d* (cons other d*))
		       (loop other* (- reset 1) (- reset 1)))
		      ((define-syntax)
		       (set! top-level-box
			     (extend-boxed-env (cadr other)
					       (make-macro-binding
						(parse-syntax-rules
						 (caddr other)
						 top-level-box)
						#f) ;should never be accessed
					       top-level-box))
		       (loop other* (- reset 1) (- reset 1)))
		      ((begin)
		       (let* ((new-other* (append (cdr other) other*))
			      (reset (+ (length new-other*) 1)))
			 (loop new-other* reset reset)))
		      ((load)
		       (let* ((new-other* (append (file->list (cadr other)) other*))
			      (reset (+ (length new-other*) 1)))
			 (loop new-other* reset reset)))
		      (else
		       (if (zero? count)
			   (begin
			     (set! not-recognized* (cons other other*)))
			   (loop (append other* (list other))
				 (- count 1) reset)))))))
		(error "bad toplevel expression" other)))
	  (set! not-recognized* '())))
    ;; bind the macros
    (let ((macro-symtab (unbox-env top-level-box)))
      (for-each-env! (lambda (entry)
		       (macro-binding->env! entry macro-symtab))
		     macro-symtab))
    ;; start renaming
    (set! *scheme-rename-counter* '())
    (values
     (map (lambda (d)
	    ;;(display "scheme-rename-variables: ") (display (caadr d)) (newline)
	    (let* ((definer (car d))
		   (template (cadr d))
		   (is-proc-def (pair? template))
		   (fname (if is-proc-def (car template) template))
		   (formals (if is-proc-def (cdr template) '()))
		   (new-formals (map scheme-rename-clone formals))
		   (current-box
		    (fresh-boxed-env* formals new-formals top-level-box))
		   (body-list (cddr d))
		   (body (scheme-body-list->body body-list current-box))
		   (new-body (scheme-rename-variables current-box body))
		   (new-template (if is-proc-def (cons fname new-formals) fname)))
	      `(,definer ,new-template ,new-body)))
	  d*)
     not-recognized*)))

(define (scheme-lookup-tag symtab* tag)
  (let loop ((symtab* symtab*) (tag tag))
    (cond
     ((syntax-pop-mark? tag)
      (loop (syntax-marked-env tag) (syntax-marked-exp tag)))
     ((symbol? tag)
      (apply-boxed-env symtab* tag (lambda () #f)))
     (else
      #f))))

(define (enter-binding key val env)
  (if (syntax-pop-mark? key)
      (extend-boxed-env
       (syntax-marked-exp key)
       val
       (syntax-marked-env key))
      (extend-boxed-env
       key
       val
       env)))

(define (remove-binding key env)
  (if (syntax-pop-mark? key)
      (shrink-boxed-env (syntax-marked-env key))
      (shrink-boxed-env env)))

(define (scheme-rename-variables symtab* e)
  (let loop ((e e))
    ;; (display-line "loop: " e)
    (cond
     ((syntax-pop-mark? e)
      (scheme-rename-variables (syntax-marked-env e) (syntax-marked-exp e)))
     ((symbol? e)
      (let ((found (apply-boxed-env symtab* e (lambda () #f))))
	;;(display-line "symbol-e= " e " found= "
	;; (map (lambda (symtab) (apply-env symtab e (lambda () #f))) symtab*))
	(cond
	 ((symbol? found)
	  found)
	 ((macro-binding? found)
	  (error "macro binding used as variable"))
	 ((arity-binding? found)
	  (loop (arity-binding->eta found)))
	 (else				;not found
	  e))))
     ((not (pair? e))			;some unquoted literal
      e)
     (else				;at this point all marks are stripped? no...
      (let* ((tag (syntax-car e))
	     (args (syntax-cdr e))
	     (found (scheme-lookup-tag symtab* tag)))
	(cond
	 ((macro-binding? found)
	  (let* ((macro-symtab* (make-boxed-env (macro-binding->env found)))
		 (expansion ((macro-binding->transformer found)
			     (syntax-make-env-mark e symtab*)
			     macro-symtab*)))
	    (scheme-rename-variables macro-symtab* expansion))) ;in the current environment
	 ((arity-binding? found)
	  (cons (arity-binding->name found) (syntax-map loop args)))
	 (found
	  (cons found (syntax-map loop args)))
	 ;;; need to strip the tag here
	 ((syntax-pair? tag)
	  (let ((new-tag (loop tag))
		(new-args (syntax-map loop args)))
	    (cons new-tag new-args)))
	 ((syntax-eq-symbol? 'QUOTE tag symtab*)
	  ;;(display "!!!Q1 ") (display e) (newline)
	  (syntax-strip-recursively e))
	 ((syntax-eq-symbol? 'QUASIQUOTE tag symtab*)
	  (loop (backquote-expander 0 (syntax-car args))))
	 ;; named let
	 ((and (syntax-eq-symbol? 'LET tag symtab*)
	       (not (syntax-pair? (syntax-car args)))
	       (not (syntax-null? (syntax-car args))))
	  ;;(display-line "named-let: " args)
	  (letrec ((list-last (lambda (l) (if (and (pair? l) (null? (cdr l)))
					      (car l)
					      (list-last (cdr l))))))
	    (scheme-rename-variables
	     (empty-boxed-env)
	     (or ((syntax-rules-transformer
		   '()
		   '(((let name ((v e) ...) body ...)
		      (letrec ((name (lambda (v ...) body ...)))
			(name e ...))))
		   (lambda () #f))
		  (syntax-make-env-mark (cons tag args) symtab*)
		  symtab*)
		 (error "syntax error in named let" e)))))
	 ;; !!! need to strip off SCHEME-POP-MARK
	 ((syntax-eq-symbol? 'LET tag symtab*)
	  ;;(display-line "let: " args)
	  (let* ((bindings (syntax-car args))
		 (formals (syntax-map (lambda (binding) (syntax-car binding))
				      bindings))
		 (bodies (syntax-map (lambda (x) (syntax-car (syntax-cdr x)))
				     bindings))
		 (body-list (syntax-cdr args))
		 (renamed-formals (map scheme-rename-clone formals))
		 (new-bodies (map loop bodies)))
	    ;; bind each variable in its own env
	    (for-each (lambda (formal renamed-formal)
			(enter-binding formal renamed-formal symtab*))
		      formals renamed-formals)
	    (let ((new-body (loop (scheme-body-list->body body-list symtab*))))
	      ;; leaving scope: unbind variables
	      (for-each (lambda (formal)
			  (remove-binding formal symtab*))
			formals)
	      ;; build expression
	      (let loop ((new-formals renamed-formals)
			 (new-bodies new-bodies))
		(if (null? new-formals)
		    new-body
		    `(LET ((,(car new-formals) ,(car new-bodies)))
		       ,(loop (cdr new-formals) (cdr
						 new-bodies))))))))
	 ;;
	 ((syntax-eq-symbol? 'LETREC tag symtab*)
	  (let* ((bindings (syntax-car args))
		 (formals (syntax-map (lambda (binding) (syntax-car binding))
				      bindings))
		 (bodies (syntax-map (lambda (x) (syntax-car (syntax-cdr x))) bindings))
		 (body-list (syntax-cdr args))
		 (new-formals (map scheme-rename-clone formals))
		 ;; assume [no longer that] all bodies are lambdas
		 (arities (syntax-map
			   (lambda (body)
			     (and (syntax-pair? body)
				  (or (syntax-eq-symbol?
				       'LAMBDA (syntax-car body) symtab*)
				      (syntax-eq-symbol?
				       'LAMBDA-WITHOUT-MEMOIZATION
				       (syntax-car body) symtab*))
				  (syntax-car (syntax-cdr body))))
			   bodies))
		 (munge (lambda (new-formal arity)
			  (if arity
			      (make-arity-binding
			       new-formal
			       (let ((arity (syntax-strip arity)))
				 `(,(syntax-make-env-mark 'LAMBDA (empty-boxed-env))
				   ,arity (,new-formal ,@arity))))
			      new-formal))))
	    ;;(display-line "letrec: " formals)
	    ;; bind each variable in its own env
	    (for-each (lambda (formal renamed-formal arity)
			(enter-binding formal (munge renamed-formal arity) symtab*))
		      formals new-formals arities)
	    ;; construct bodies
	    (let ((new-bodies (map loop bodies))
		  (new-body (loop (scheme-body-list->body body-list symtab*))))
	      ;; unbind variables
	      (for-each (lambda (formal)
			  (remove-binding formal symtab*))
			formals)
	      ;; build result
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
					      ,final-body)))))))))))
	 ;;
	 ((or (syntax-eq-symbol? 'LAMBDA tag symtab*)
	      (syntax-eq-symbol? 'LAMBDA-WITHOUT-MEMOIZATION tag symtab*)
	      (syntax-eq-symbol? 'LAMBDA-POLY tag symtab*))
	  (let* ((formals (syntax-car args))
		 (new-formals (let loop ((formals formals))
				(cond
				 ((syntax-pair? formals)
				  (cons (scheme-rename-clone (syntax-car formals))
					(loop (syntax-cdr formals))))
				 ((syntax-null? formals)
				  '())
				 (else
				  (scheme-rename-clone formals))))))
	    ;;(display-line "lambda: " formals)
	    (let loop ((formals formals) (new-formals new-formals))
	      (cond
	       ((pair? new-formals)
		(enter-binding (syntax-car formals)
			       (car new-formals)
			       symtab*)
		(loop (syntax-cdr formals) (cdr new-formals)))
	       ((null? new-formals))
	       (else
		(enter-binding formals new-formals symtab*))))
	    (let* ((body-list (syntax-cdr args))
		   (body (scheme-body-list->body body-list symtab*))
		   (new-body (loop body)))
	      ;; unbind
	      (let loop ((formals formals))
		(cond
		 ((syntax-pair? formals)
		  (remove-binding (syntax-car formals) symtab*)
		  (loop (syntax-cdr formals)))
		 ((syntax-null? formals))
		 (else
		  (remove-binding formals symtab*))))
	      ;; result
	      ;;(display-line "lambda returns: " `(,tag ,new-formals ,new-body))
	      `(,(syntax-strip tag) ,new-formals ,new-body))))
	 ((syntax-eq-symbol? 'SET! tag symtab*)
	  ;;(display-line "set!: " args)
	  (let ((renamed-args (syntax-map loop args)))
	    (scheme->abssyn-mutable-variable! (syntax-strip (syntax-car renamed-args)))
	    `(SET! ,@renamed-args)))
	 ;; local macros
	 ;; (let-syntax ((v syntax-rules)) body)
	 ((syntax-eq-symbol? 'LET-SYNTAX tag symtab*)
	  (let* ((phrase (syntax-car (syntax-car args)))
		 (formal (syntax-car phrase)))
	    ;; bind macro
	    (enter-binding formal
			   (make-macro-binding
			    (parse-syntax-rules
			     (syntax-car (syntax-cdr phrase))
			     symtab*)
			    (unbox-env symtab*))
			   symtab*)
	    (let* ((body-list (syntax-cdr args))
		   (body (scheme-body-list->body body-list symtab*))
		   (new-body (loop body)))
	      ;; unbind macro
	      (remove-binding formal symtab*)
	    new-body)))
	 ;;(letrec-syntax ((...)...) body)
	 ((syntax-eq-symbol? 'LETREC-SYNTAX tag symtab*)
	  (let* ((bindings (syntax-car args))
		 (formals (syntax-map (lambda (x) x) bindings))
		 (rules (syntax-map (lambda (x) (syntax-car (syntax-cdr x)))
				    bindings))
		 (macro-expanders (map (lambda (rule-set)
					 (make-macro-binding
					  (parse-syntax-rules
					   rule-set
					   symtab*)
					  #f))
				       rules)))
	    ;; bind the macros
	    (for-each (lambda (formal macro-expander)
			(enter-binding formal macro-expander symtab*))
		      formals macro-expanders)
	    ;; tie the recursive knot
	    (for-each (lambda (macro-expander)
			(macro-binding->env! macro-expander (unbox-env symtab*)))
		      macro-expanders)
	    (let* ((body-list (syntax-cdr args))
		   (new-body (loop (scheme-body-list->body body-list symtab*))))
	      ;; unbind
	      (for-each (lambda (formal)
			  (remove-binding formal symtab*))
			formals)
	      new-body)))
	 (else
	  ;;(display-line "tag= " tag)
	  (let ((result (cons (syntax-strip tag)
			      (syntax-map loop args))))
	    ;;(display-line "function call: " result)
	    result))))))))

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
  (let ((wrapper (lambda (x) (syntax-make-env-mark x (empty-boxed-env)))))
    (if (= 1 (length (syntax-strip body-list)))
	(syntax-car body-list)
	(let loop ((body-list body-list) (definitions '()))
	  (if (and (syntax-pair? body-list)
		   (syntax-pair? (syntax-car body-list))
		   (let ((sym (syntax-car (syntax-car body-list))))
		     (or (syntax-eq-symbol? 'DEFINE sym symtab*)
			 (syntax-eq-symbol?
			  'DEFINE-WITHOUT-MEMOIZATION
			  sym symtab*))))
	      (loop (syntax-cdr body-list)
		    (cons (syntax-car body-list) definitions))
	      (let ((real-body (if (= 1 (length (syntax-strip body-list)))
				   (syntax-car body-list)
				   `(,(wrapper 'BEGIN)
				     ,@(syntax-map (lambda (x) x) body-list))))
		    (def->letrec-clause
		      (lambda (def)
			(let ((template
			       (syntax-car (syntax-cdr def)))
			      (inner-body-list
			       (syntax-cdr (syntax-cdr def)))
			      (abstract
			       (if (syntax-eq-symbol? 'DEFINE (syntax-car def) symtab*)
				   'LAMBDA
				   'LAMBDA-WITHOUT-MEMOIZATION)))
			  (if (syntax-pair? template)
			      `(,(syntax-car template)
				(,(wrapper abstract)
				 ,(syntax-cdr template)
				 ,@(syntax-map (lambda (x) x)
					       inner-body-list)))
			      (let ((body (syntax-car
					   inner-body-list)))
				(if (and (syntax-pair? body)
					 (syntax-eq-symbol?
					  'LAMBDA (syntax-car body)
					  symtab*))
				    (set-car! body (wrapper abstract)))
				`(,template ,body)))))))
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
	 ((or (eq? tag 'LAMBDA)
	      (eq? tag 'LAMBDA-WITHOUT-MEMOIZATION)
	      (eq? tag 'LAMBDA-POLY))
	  (let* ((bound-vars (car args))
		 (wrapped-body (scheme-wrap-e (cadr args))))
	    (scheme-wrap-binding bound-vars
				 wrapped-body
				 (lambda (new-vars body)
				   `(,tag (,@new-vars) ,body)))))
	 (else
	  (cons tag (map scheme-wrap-e args)))))))

;;; second step: perform lambda lifting for LETREC forms, assumes
;;; first step has been performed


(define *scheme-lambda-lift-definitions* '())
(define (scheme-lambda-lift-add-definition d)
  (set! *scheme-lambda-lift-definitions*
	(cons d *scheme-lambda-lift-definitions*)))
(define (scheme-lambda-lift-d d*)
  (define (scheme-lambda-lift-one-d d)
    (let ((definer (car d))
	  (template (cadr d)))
      `(,definer ,template
		 ,(scheme-lambda-lift (caddr d)
				      (if (pair? template)
					  (cdr template)
					  '())
				      definer))))
  
  (set! *scheme-lambda-lift-definitions* '())
  (let ((old-d* (map scheme-lambda-lift-one-d d*)))
    (append old-d* *scheme-lambda-lift-definitions*)))


(define (lambda->definer symbol definer)
  (if (eq? symbol 'lambda)
      definer
      'define-without-memoization))

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
		 (lambdas (map (lambda (h) (caadr h)) headers))
		 (call-graph (map cons bound-vars dependencies))
		 (free-vars (map (lambda (h)
				   (scheme-freevars (cadr h) vars))
				 headers))
		 (b/free-vars (map cons bound-vars free-vars))
		 (b/free-vars (scheme-lambda-lift-fix call-graph b/free-vars))
		 ;;(new-vars (append bound-vars vars))
		 ;;(fv* (apply set-union* free-vars))
		 (bound-bodies
		  (map (lambda (h lam)
			 (scheme-lambda-lift
			  (scheme-lambda-lift-vars b/free-vars
						   (cadr h))
			  vars
			  (lambda->definer lam definer)))
		       headers
		       lambdas)))
	    (map (lambda (b/fv* bb lam)
		   (scheme-lambda-lift-add-definition
		    `(,(lambda->definer lam definer) (,@b/fv* ,@(cadr bb))
		       ,(caddr bb))))
		 b/free-vars bound-bodies lambdas)
	    (scheme-lambda-lift
	     (scheme-lambda-lift-vars b/free-vars body)
	     vars
	     definer)))
	 ;;
	 ((or (eq? tag 'LAMBDA)
	      (eq? tag 'LAMBDA-WITHOUT-MEMOIZATION)
	      (eq? tag 'LAMBDA-POLY))
	  (let* ((bound-vars (car args))
		 (vars (append (scheme-formals->vars bound-vars) vars))
		 (body (cadr args)))
	    `(,tag ,bound-vars
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
	 ((or (eq? tag 'LAMBDA)
	      (eq? tag 'LAMBDA-WITHOUT-MEMOIZATION)
	      (eq? tag 'LAMBDA-POLY))
	  (let* ((bound-vars (scheme-formals->vars (car args)))
		 (vars (append bound-vars vars))
		 (body (cadr args))) 
	    (set-difference (scheme-freevars body vars)
			    bound-vars)))
	 (else
	  (apply set-union*
		 (map (lambda (e) (scheme-freevars e vars)) args)))))))


