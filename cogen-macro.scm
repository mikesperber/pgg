;;; macro expansion using syntax-rules

(define repeated?
  (lambda (pattern)
    (and (pair? (cdr pattern)) (eq? '... (cadr pattern)))))

;;; an environment maps a pattern variable to (multiplicity value)
;;; where multiplicity indicates the nesting depth of list constructors
;;; in value, i.e., value = list^multiplicity (primitive value)

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
	  ((null? pattern)
	   (if (syntax-null? subject)
	       '()
	       (fail)))
	  ((symbol? pattern)
	   (if (memq pattern literals)
	       (if (syntax-eq-symbol? pattern subject symtab*) '() (fail))
	       `((,pattern 0 ,subject))))
	  ((pair? pattern)
	   (if (repeated? pattern)
	       (if (syntax-list? subject)
		   (if (syntax-null? subject)
		       (build-empty-env pattern)
		       (apply map combine
			      (syntax-map (lambda (car-subject) (loop (car pattern) car-subject))
					  subject)))
		   (fail))
	       (if (syntax-pair? subject)
		   (append (loop (car pattern) (syntax-car subject))
			   (loop (cdr pattern) (syntax-cdr subject)))
		   (fail))))
	  ((vector? pattern)
	   (if (syntax-vector? subject)
	       (loop (vector->list pattern) (syntax-vector->list subject))
	       (fail)))
	  (else
	   (or (syntax-eq? pattern subject) (fail)))))))))

(define (instantiate template env literals)

  (define (instantiate-internal template level env)
    (letrec
	((pattern-vars
	  (lambda (template penv)
	    (cond
	     ((null? template)
	      penv)
	     ((symbol? template)
	      (let ((found (assoc template penv)))
		(if found
		    penv
		    (let ((found (assoc template env)))
		      (if found
			  (cons found penv)
			  penv)))))
	     ((pair? template)
	      (pattern-vars (car template) (pattern-vars (cdr template) penv)))
	     (else
	      penv)))))
      (let loop ((template template))
	(cond
	 ((null? template)
	  '())
	 ((symbol? template)
	  (let ((found (assoc template env)))
	    (cond
	     (found
	      (if (zero? (cadr found))
		  (caddr found)
		  (error "template multiplicity error (bad nesting of ...s)" template)))
	     ((memq template literals)
	      template)
	     (else			;a free variable
	      template))))
	 ((pair? template)
	  (if (repeated? template)
	      (let ((penv (pattern-vars (car template) '())))
		(append
		 (if (null? penv)
		     (error "cannot determine replication (too many ...s)" template)
		     (begin
		       (map (lambda (new-env)
			      (instantiate-internal (car template) (+ 1 level) new-env))
			    (apply map
				   (lambda values
				     (map (lambda (pentry value)
					    `(,(car pentry)
					      ,(- (cadr pentry) 1)
					      ,value))
					  penv values))
				   (map caddr penv)))))
		 (loop (cddr template))))
	      (cons (loop (car template))
		    (loop (cdr template)))))
	 ((vector? template)
	  (list->vector (loop (vector->list template))))
	 (else
	  template)))))

  (instantiate-internal template 0 env))



(define SCHEME-POP-MARK (list(string->symbol "Scheme-Pop-Mark")))
(define (syntax-make-pop-mark exp)
  (list SCHEME-POP-MARK exp))
(define (syntax-marked-exp exp)
  (cadr exp))
(define (syntax-pop-mark? exp)
  (and (pair? exp) (eq? (car exp) SCHEME-POP-MARK)))


(define (syntax-null? exp)
  (null? (syntax-strip exp)))
(define (syntax-pair? exp)
  (pair? (syntax-strip exp)))
(define (syntax-list? exp)
  (list? (syntax-strip exp)))
(define (syntax-vector? exp)
  (vector? (syntax-strip exp)))
(define (syntax-eq? pat exp)
  (eq? pat (syntax-strip exp)))
(define (syntax-eq-symbol? pat exp symtab*)
  (let loop ((exp exp) (symtab* symtab*))
    (if (syntax-pop-mark? exp)
	(loop (syntax-marked-exp exp) (cdr symtab*))
	(and (symbol? exp)
	     (let ((found (apply-env (car symtab*) exp (lambda () #f))))
	       (and (not found)
		    (eq? pat exp)))))))
(define (syntax-map f exp)
  (let loop ((exp exp) (mark (lambda (x) x)))
    (if (syntax-pop-mark? exp)
	(loop (syntax-marked-exp exp) (lambda (x) (mark (syntax-make-pop-mark x))))
	(map (lambda (e) (f (mark e))) exp))))
(define (syntax-vector->list exp)
  (let loop ((exp exp) (mark (lambda (x) x)))
    (if (syntax-pop-mark? exp)
	(loop (syntax-marked-exp exp) (lambda (x) (mark (syntax-make-pop-mark x))))
	(map mark (vector->list exp)))))
(define (syntax-car exp)
  (if (syntax-pop-mark? exp)
      (syntax-make-pop-mark (syntax-car (syntax-marked-exp exp)))
      (car exp)))
(define (syntax-cdr exp)
  (if (syntax-pop-mark? exp)
      (syntax-make-pop-mark (syntax-cdr (syntax-marked-exp exp)))
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


;;;
;;; parser for syntax-rules
;;; <exp> is (syntax-rules ...)
;;; <exp> -> transformer or #f
;;;
(define (parse-syntax-rules exp)
  (cond
   ((eq? (car exp) 'syntax-rules)
    (let ((literals (cadr exp))
	  (rules (cddr exp)))
      (syntax-rules-transformer literals
				rules
				(lambda () (error "No match for macro call" exp)))))
   (else
    #f)))

(define (syntax-rules-transformer literals rules fail)
  (lambda (exp dynamic-symtab*)		;<<- transformer
    (let loop ((rules rules))
      (if (null? rules)
	  (fail)
	  (let* ((rule (car rules))
		 (env (build-env (car rule) exp literals dynamic-symtab*)))
	    (if env
		(instantiate (cadr rule) env literals)
		(loop (cdr rules))))))))
