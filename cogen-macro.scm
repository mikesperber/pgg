;;; cogen-macro
;;; macro expansion using syntax-rules

;;; copyright © 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

(define repeated?
  (lambda (pattern)
    (and (syntax-pair? (syntax-cdr pattern))
	 (syntax-eq-symbol? '... (cadr pattern) (empty-boxed-env)))))

;;; an environment maps a pattern variable to (multiplicity value)
;;; where multiplicity indicates the nesting depth of list constructors
;;; in value, i.e., value = list^multiplicity (primitive value)

;;; ATTENTION: pattern and template may also contain marks!

;;; match <pattern> against <subject>
;;; treating <literals> as literal symbols
;;; returns #f if matching fails, otherwise an environment as described above
;;; needs the call time symbol table <symtab>
(define (build-env pattern subject literals symtab*)
  (call-with-current-continuation
   (lambda (exit)
     (letrec
	 ((fail
	   (lambda ()
	     (exit #f)))
	  (combine
	   (lambda (binding . bindings)	;not empty
	     `(,(car binding)
	       ,(+ 1 (cadr binding))
	       ,(map caddr (cons binding bindings)))))
	  (build-empty-env
	   (lambda (pattern)
	     (cond
	      ((null? pattern)
	       '())
	      ((symbol? pattern)
	       (if (memq pattern literals)
		   '()
		   `((,pattern 0 ()))))
	      ((pair? pattern)
	       (if (repeated? pattern)
		   (map (lambda (binding)
			  `(,(car binding) ,(+ 1 (cadr binding)) ()))
			(build-empty-env (car pattern)))
		   (append (build-empty-env (car pattern))
			   (build-empty-env (cdr pattern)))))
	      (else
	       '())))))
       (let loop ((pattern pattern) (subject subject))
	 (cond
	  ((syntax-null? pattern)
	   (if (syntax-null? subject)
	       '()
	       (fail)))
	  ((syntax-symbol? pattern)
	   (if (syntax-memq pattern literals symtab*)
	       (if (syntax-eq-symbol? pattern subject symtab*) '() (fail))
	       `((,pattern 0 ,subject))))
	  ((syntax-pair? pattern)
	   (if (repeated? pattern)
	       (if (syntax-list? subject)
		   (if (syntax-null? subject)
		       (build-empty-env pattern)
		       (apply map combine
			      (syntax-map (lambda (car-subject)
					    (loop (syntax-car pattern) car-subject))
					  subject)))
		   (fail))
	       (if (syntax-pair? subject)
		   (append (loop (syntax-car pattern) (syntax-car subject))
			   (loop (syntax-cdr pattern) (syntax-cdr subject)))
		   (fail))))
	  ((syntax-vector? pattern)
	   (if (syntax-vector? subject)
	       (loop (syntax-vector->list pattern) (syntax-vector->list subject))
	       (fail)))
	  (else
	   (if (syntax-eq? pattern subject)
	       '()
	       (fail)))))))))

(define (instantiate template env literals symtab*)

  (define (instantiate-internal template level env)
    (letrec
	((pattern-vars
	  (lambda (template penv)
	    (cond
	     ((syntax-null? template)
	      penv)
	     ((syntax-symbol? template)
	      (let ((found (syntax-assoc template penv symtab*)))
		(if found
		    penv
		    (let ((found (syntax-assoc template env symtab*)))
		      (if found
			  (cons found penv)
			  penv)))))
	     ((syntax-pair? template)
	      (pattern-vars (syntax-car template)
			    (pattern-vars (syntax-cdr template) penv)))
	     (else
	      penv)))))
      (let loop ((template template))
	(cond
	 ((syntax-null? template)
	  '())
	 ((syntax-symbol? template)
	  (let ((found (syntax-assoc template env symtab*)))
	    (cond
	     (found
	      (if (zero? (cadr found))
		  (caddr found)
		  (error "template multiplicity error (bad nesting of ...s)" template)))
	     ((syntax-memq template literals symtab*)
	      template)
	     (else			;a free variable
	      template))))
	 ((syntax-pair? template)
	  (if (repeated? template)
	      (let ((penv (pattern-vars (syntax-car template) '())))
		(append
		 (if (null? penv)
		     (error "cannot determine replication (too many ...s)" template)
		     (begin
		       (map (lambda (new-env)
			      (instantiate-internal (syntax-car template)
						    (+ 1 level)
						    new-env))
			    (apply map
				   (lambda values
				     (map (lambda (pentry value)
					    `(,(car pentry)
					      ,(- (cadr pentry) 1)
					      ,value))
					  penv values))
				   (map caddr penv)))))
		 (loop (syntax-cdr (syntax-cdr template)))))
	      (cons (loop (syntax-car template))
		    (loop (syntax-cdr template)))))
	 ((syntax-vector? template)
	  (list->vector (loop (syntax-vector->list template))))
	 (else
	  template)))))

  (instantiate-internal template 0 env))



(define SCHEME-POP-MARK (list(string->symbol "Scheme-Pop-Mark")))
(define (syntax-make-env-mark exp env)
  (if (syntax-pop-mark? exp)
      exp
      (list SCHEME-POP-MARK exp env)))
(define (syntax-marked-env exp)
  (caddr exp))
(define (syntax-make-pop-mark exp)
  (list SCHEME-POP-MARK exp))
(define (syntax-marked-exp exp)
  (cadr exp))
(define (syntax-pop-mark? exp)
  (and (pair? exp) (eq? (car exp) SCHEME-POP-MARK)))


(define (syntax-null? exp)
  (null? (syntax-strip exp)))
(define (syntax-symbol? exp)
  (symbol? (syntax-strip exp)))
(define (syntax-pair? exp)
  (pair? (syntax-strip exp)))
(define (syntax-list? exp)
  (list? (syntax-strip exp)))
(define (syntax-vector? exp)
  (vector? (syntax-strip exp)))
(define (syntax-eq? pat exp)
  (let ((stripped-exp (syntax-strip exp)))
    (or (eq? pat stripped-exp)
	(equal? pat stripped-exp))))
(define (syntax-memq x literals symtab*)
  (let loop ((literals literals))
    (and (pair? literals)
	 (or (syntax-eq-symbol? x (car literals) symtab*)
	     (loop (cdr literals))))))
(define (syntax-assoc x alist symtab*)
  (let loop ((alist alist))
    (and (pair? alist)
	 (let ((apair (car alist)))
	   (if (syntax-eq-symbol? x (car apair) symtab*)
	       apair
	       (loop (cdr alist)))))))
(define (syntax-lookup-symbol exp symtab*)
  (if (syntax-pop-mark? exp)
      (syntax-lookup-symbol (syntax-marked-exp exp)
			    (syntax-marked-env exp))
      (or (apply-boxed-env symtab* exp (lambda () #f))
	  exp)))
(define (syntax-eq-symbol? pat exp symtab*)
  (and (syntax-symbol? pat)
       (syntax-symbol? exp)
       (eq? (syntax-lookup-symbol pat symtab*)
	    (syntax-lookup-symbol exp symtab*))))
(define (syntax-eq-symbol?-obsolete pat exp symtab*)
  (let loop ((exp exp) (symtab* symtab*))
    (if (syntax-pop-mark? exp)
	(loop (syntax-marked-exp exp) (syntax-marked-env exp))
	(and (symbol? exp)
	     (let ((found (apply-boxed-env symtab* exp (lambda () #f))))
	       (and (not found)
		    (eq? pat exp)))))))
(define (syntax-map f exp)
  (let loop ((exp exp) (mark (lambda (x) x)))
    (if (syntax-pop-mark? exp)
	(loop (syntax-marked-exp exp)
	      (let ((env (syntax-marked-env exp)))
		(lambda (x) (mark (syntax-make-env-mark x env)))))
	(map (lambda (e) (f (mark e))) exp))))
(define (syntax-vector->list exp)
  (let loop ((exp exp) (mark (lambda (x) x)))
    (if (syntax-pop-mark? exp)
	(loop (syntax-marked-exp exp)
	      (let ((env (syntax-marked-env exp)))
		(lambda (x) (mark (syntax-make-env-mark x env)))))
	(map mark (vector->list exp)))))
(define (syntax-car exp)
  (if (syntax-pop-mark? exp)
      (syntax-make-env-mark (syntax-car (syntax-marked-exp exp))
			    (syntax-marked-env exp))
      (car exp)))
(define (syntax-cdr exp)
  (if (syntax-pop-mark? exp)
      (syntax-make-env-mark (syntax-cdr (syntax-marked-exp exp))
			    (syntax-marked-env exp))
      (cdr exp)))
(define (syntax-depth exp)
  (cond
   ((syntax-pop-mark? exp)
    (+ 1 (syntax-depth (syntax-marked-exp exp))))
   ((pair? exp)
    (max (syntax-depth (car exp))
	 (syntax-depth (cdr exp))))
   (else
    0)))
(define (syntax-strip exp)
  (if (syntax-pop-mark? exp)
      (syntax-strip (syntax-marked-exp exp))
      exp))
(define (syntax-strip-recursively exp)
  (let loop ((exp exp))
    (let ((stripped (syntax-strip exp)))
      (cond
       ((pair? stripped)
	(cons (loop (car exp)) (loop (cdr exp))))
       (else
	stripped)))))


;;;
;;; parser for syntax-rules
;;; <exp> is (syntax-rules ...)
;;; <exp> -> transformer or #f
;;;
(define (parse-syntax-rules exp symtab*)
  (cond
   ((syntax-eq-symbol? 'syntax-rules (car exp) symtab*)
    (let* ((rest (syntax-cdr exp))
	   (literals (syntax-map (lambda (x) x) (syntax-car rest)))
	   (rules (syntax-cdr rest)))
      (syntax-rules-transformer literals
				rules
				(lambda () (error "No match for macro call" exp)))))
   (else
    #f)))

(define (syntax-rules-transformer literals rules fail)
  (lambda (exp dynamic-symtab*)		;<<- transformer
    (let loop ((rules rules))
      (if (syntax-null? rules)
	  (fail)
	  (let* ((rule (syntax-car rules))
		 (stripped-rule (syntax-strip rule)))
	    (cond
	     ((and (list? stripped-rule) (= 2 (length stripped-rule)))
	      (let ((env (build-env (syntax-cdr (syntax-car rule))
				    (syntax-cdr exp)
				    literals dynamic-symtab*)))
		(if env
		    (instantiate (syntax-car (syntax-cdr rule)) env
				 literals
				 dynamic-symtab*)
		    (loop (syntax-cdr rules)))))
	     ((eq? stripped-rule 'else)
	      ;; avoid recursive application
	      (cons (syntax-make-env-mark (syntax-strip (syntax-car exp))
					  (empty-boxed-env))
		    (syntax-cdr exp)))
	     (else
	      (display "syntax-rules: syntax error in ")
	      (display stripped-rule)
	      (newline)
	      (fail))))))))
