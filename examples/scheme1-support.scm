;;; scheme1.adt

;;; preprocessed and simplified abstract syntax

;;; E ::= V | K | (if E1 E2 E3)  | (call P E*) | (O E*) |
;;;       (let ((V E)) E) | (lambda (V) E) | (apply E1 E2) 

(define (s1-is-V? E) (symbol? E))
(define (s1-is-value? exp)
  (or (number? exp)
      (char? exp)
      (boolean? exp)
      (and (list? exp) (null? exp))))
(define (s1-is-quote? exp)
  (if (list? exp)
      (equal? (car exp) (quote quote))
      #f))
(define (s1-is-if? exp)
  (if (list? exp)
      (equal? (car exp) (quote if))
      #f))
(define (s1-is-let? exp)
  (if (list? exp)
      (equal? (car exp) (quote let))
      #f))
(define (s1-is-call? exp)
  (if (list? exp)
      (equal? (car exp) (quote call))
      #f))
(define (s1-is-lambda? exp)
  (if (list? exp)
      (equal? (car exp) (quote lambda))
      #f))
(define (s1-is-apply? exp)
  (if (list? exp)
      (equal? (car exp) (quote apply))
      #f))
(define (s1-get-quote-body E) (list-ref E 1))
(define (s1-get-if-cond E) (list-ref E 1)) 
(define (s1-get-if-then E) (list-ref E 2)) 
(define (s1-get-if-else E) (list-ref E 3)) 
(define (s1-get-let-variable E) (caaadr E))
(define (s1-get-let-bound-expression E) (car (cdaadr E)))
(define (s1-get-let-expression E) (list-ref E 2))
(define (s1-get-call-proc E) (list-ref E 1)) 
(define (s1-get-call-args E) (cddr E)) 
(define (s1-get-op-name E) (list-ref E 0)) 
(define (s1-get-op-args E) (cdr E)) 
(define (s1-get-lambda-arg E) (car (list-ref E 1))) 
(define (s1-get-lambda-body E) (list-ref E 2)) 

(define (s1-get-apply-rator E) (list-ref E 1)) 
(define (s1-get-apply-rand E) (list-ref E 2)) 

;;; program manipulation

;;; function definition lookup 
(define (s1-lookup-defn P N)
  (if (equal? (s1-get-proc-name (car P)) N)
      (car P)
      (s1-lookup-defn (cdr P) N)))

(define (s1-get-proc-name D) (caadr D))
(define (s1-get-proc-args D) (cdadr D))
(define (s1-get-proc-body D) (caddr D))

(define (s1-get-proc-args-flow D) (cdr (cadddr D)))
(define (s1-get-proc-body-flow D) (car (cadddr D)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list.adt
;;;(defconstr
;;;  (my-cons my-car my-cdr)
;;;  (my-nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; closure.adt
;;;(defconstr
;;;  (mk-error)
;;;  (mk-value   get-value)
;;;  (mk-closure closure-nr closure-vars)
;;;  (mk-data    data-nr    data-args)
;;;  (mk-constant constant-places constant-arg))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flow.adt
;;;
;;; do flow analysis of Scheme1
;;;
;;; $Header $
;;; $Log $
;;;
;;;(loadt "scheme1.adt")

;;; (s1-flow-analysis defn*)
;;;	performs flow analysis of the program `defn*'
;;;	output: a partition of all program points
;;; (s1-flow-analysis defn* 'raw)
;;;	output: constraint set

(define (s1-flow-constraints D*)
  (let ((n 0))
;;; 1. provide a flow variable at every node

(define s1-gen-flow-var
  (lambda () (begin (set! n (+ n 1)) n)))

(define (s1-flow-intro-defns D*)
  ;;(display "s1-flow-intro-defns ") (display D*) (newline)
  (if (null? D*)
      '()
      (let ((D (car D*)))
	(cons `(define (,(s1-get-proc-name D) ,@(s1-get-proc-args D))
		 ,(s1-flow-intro (s1-get-proc-body D))
		 (,(s1-gen-flow-var)
		  ,@(map (lambda (z) (s1-gen-flow-var)) (s1-get-proc-args D))))
	      (s1-flow-intro-defns (cdr D*))))))

(define (s1-flow-intro E)
  (cons (s1-gen-flow-var)
	(cond
	 ((or (not (list? E))
	       (null? E)
	       (equal? 'quote (car E)))
	   E)
	 ((equal? 'if (car E))
	  (list 'if
		(s1-flow-intro (s1-get-if-cond E))
		(s1-flow-intro (s1-get-if-then E))
		(s1-flow-intro (s1-get-if-else E))))
	 ((equal? 'call (car E))
	  `(call ,(s1-get-call-proc E) ,@(map s1-flow-intro (s1-get-call-args E))))
	 ((equal? 'let (car E))
	  `(let ((,(s1-flow-intro (s1-get-let-variable E)) ,(s1-flow-intro (s1-get-let-bound-expression E))))
	     ,(s1-flow-intro (s1-get-let-expression E))))
	 ((equal? 'lambda (car E))
	  `(lambda (,(s1-flow-intro (s1-get-lambda-arg E))) ,(s1-flow-intro (s1-get-lambda-body E))))
	 ((equal? 'apply (car E))
	  `(apply ,(s1-flow-intro (s1-get-apply-rator E)) ,(s1-flow-intro (s1-get-apply-rand E))))
	 (else
	  `(,(s1-get-op-name E) ,@(map s1-flow-intro (s1-get-op-args E))))
	 )))

;;; 2. introduce constraints
;;;
;;; there are four kinds of constraints:
;;; (a1) equality constraints		X = Y		  (eq X Y)
;;; (a2) procedure call constraints    X == Y for proc Z  (peq X Y Z)
;;;      convention: X is the actual, Y the formal parameter
;;; (b) flow constraints		<X -> Y> \prec Z  (fct X Y Z)
;;;   extension (not implemented): X1 .. Xn -> Y \prec Z  (fct (X1 .. Xn Y) Z)
;;; (c) data structure constr    (Ctor X1 .. Xn) \prec Z  (acons (X1 .. Xn) Z)
;;; to be interpreted in the domains
;;;	vv = ff + cc
;;;	ff = (vv \times vv)^\bot
;;;	cc = (vv \times .. \times vv)^\bot
;;; (d) need dependency constraints, too:  X \rhd Y	(dep X Y)

(define (s1-lookup-flow phi v)
  (if (equal? v (caar phi))
      (cdar phi)
      (s1-lookup-flow (cdr phi) v)))

(define (update-flow fv phi)
  (cons (cons (cdr fv) (car fv)) phi))

;;; collect constraints
(define (collect-constraints D0*)
  ;;(display "collect-constraints ") (display D*) (newline)
  (let* ((c-set '())
	 (c-add (lambda (crt) (set! c-set (cons crt c-set)))))
    (let loop ((D* D0*))
      (if (null? D*)
	  '()
      (let*
	  ((D (car D*))
	   (phi (map (lambda (v f) (cons v f))
		     (s1-get-proc-args D)
		     (s1-get-proc-args-flow D))))
	(c-add `(eq ,(s1-get-proc-body-flow D) ,(s1-flow-of (s1-get-proc-body D))))
    ;; need an env phi, mapping variables to their flow variables
    (let loop ((flow-e (s1-get-proc-body D)) (phi phi))
      (let ((flow-var (car flow-e))
	    (e (cdr flow-e)))
	;;(display "collect-constraints ") (display E) (newline)
	(cond
	 ((s1-is-V? E)
	  (c-add `(eq ,flow-var ,(s1-lookup-flow phi E))))
	 ((or (not (list? E))
	       (null? E)
	       (equal? 'quote (car E)))
	  ;(c-add `(eq ,flow-var base))
	  )
	 ((equal? 'if (car E))
	  ;(c-add `(eq ,(s1-flow-of (get-if-cond E)) base))
	  (c-add `(eq ,(s1-flow-of (s1-get-if-then E)) ,flow-var))
	  (c-add `(eq ,(s1-flow-of (s1-get-if-else E)) ,flow-var))
	  (c-add `(dep ,(s1-flow-of (s1-get-if-cond E)) ,(s1-flow-of (s1-get-if-then E))))
	  (loop (s1-get-if-cond E) phi)
	  (loop (s1-get-if-then E) phi)
	  (loop (s1-get-if-else E) phi))
	 ((equal? 'call (car E))
	  (let* ((D (s1-lookup-defn D0* (s1-get-call-proc E)))
		 (proc-body-flow (s1-get-proc-body-flow D)))
	    (map (lambda (e y) (c-add `(peq ,(s1-flow-of e) ,y ,proc-body-flow)))
		 (s1-get-call-args E)
		 (s1-get-proc-args-flow D))
	    (map (lambda (e) (loop e phi))
		 (s1-get-call-args E))
	    (c-add `(peq ,flow-var ,proc-body-flow ,proc-body-flow))))
	 ((equal? 'let (car E))
	  (c-add `(eq ,(s1-flow-of (s1-get-let-variable E))
		      ,(s1-flow-of (s1-get-let-bound-expression E))))
	  (c-add `(eq ,(s1-flow-of (s1-get-let-expression E)) ,flow-var))
	  (loop (s1-get-let-bound-expression E) phi)
	  (loop (s1-get-let-expression E) (update-flow (s1-get-let-variable E) phi)))
	 ((equal? 'lambda (car E))
	  (c-add `(fct ,(s1-flow-of (s1-get-lambda-arg E))
		       ,(s1-flow-of (s1-get-lambda-body E))
		       ,flow-var))
	  (loop (s1-get-lambda-body E) (update-flow (s1-get-lambda-arg E) phi)))
	 ((equal? 'apply (car E))
	  (c-add `(fct ,(s1-flow-of (s1-get-apply-rand E))
		       ,flow-var
		       ,(s1-flow-of (s1-get-apply-rator E))))
	  (loop (s1-get-apply-rator E) phi)
	  (loop (s1-get-apply-rand E) phi))
	 ;; handle data structures as if defined by (defconstr (acons acar acdr))
	 ((equal? 'cons (car E))
	  (c-add `(acons (,(s1-flow-of (car (s1-get-op-args E)))
			  ,(s1-flow-of (cadr (s1-get-op-args E))))
			 ,flow-var))
	  (loop (car (s1-get-op-args E)) phi)
	  (loop (cadr (s1-get-op-args E)) phi))
	 ((equal? 'car (car E))
	  (c-add `(acons (,flow-var
			  ,(s1-gen-flow-var))
			 ,(s1-flow-of (car (s1-get-op-args E)))))
	  (loop (car (s1-get-op-args E)) phi))
	 ((equal? 'cdr (car E))
	  (c-add `(acons (,flow-var
			  ,(s1-gen-flow-var))
			 ,(s1-flow-of (car (s1-get-op-args E)))))
	  (loop (car (s1-get-op-args E)) phi))
	 ((equal? 'null? (car E))
	  (c-add `(dep ,(s1-flow-of (car (s1-get-op-args E)))
		       ,flow-var))
	  (loop (car (s1-get-op-args E)) phi))
	 ;; handle basic operators
	 (else
	  (map (lambda (e)
		 (c-add `(eq ,flow-var ,(s1-flow-of e)))
		 (loop e phi))
	       (s1-get-op-args E))))))
    (loop (cdr D*))
    	)))
    c-set
))
(let* ((DF* (s1-flow-intro-defns D*))
       (CC* (collect-constraints DF*)))
  ;;(display "s1-flow-constraints: ") (display (cons DF* CC*)) (newline)
  (list DF* CC* n))))

;;; 3. solve constraint system 
 
(define (s1-flow-simplify c-set)
  (let* ((rho-acc (s1-flow-simplify1 c-set))
	 (rho* (car rho-acc))
	 (c-set (cdr rho-acc)))
    (s1-flow-simplify2 rho* c-set)))

(define (s1-flow-simplify1 c-set)
  (let loop ((rho '()) (c-set c-set) (acc '()))
    (if (null? c-set)
	(cons rho acc)
	(let* ((c0 (car c-set))
	       (t0 (car c0)))
	  (cond
	   ((equal? t0 'eq)
	    (let ((v1 (s1-lookup-equate rho (cadr c0)))
		  (v2 (s1-lookup-equate rho (caddr c0))))
	      (if (equal? v1 v2)
		  (loop rho (cdr c-set) acc)
		  (loop (cons (cons v1 v2)
			      (map (lambda (p) (if (equal? v1 (cdr p))
						   (cons (car p) v2)
						   p)) rho))
			(cdr c-set) acc))))
	   (else
	    (loop rho (cdr c-set) (cons c0 acc))))))))

(define (s1-flow-simplify2 rho* c-set)
  (let loop ((rho rho*) (c-set c-set) (acc '()))
    (if (null? c-set)
	(cons rho acc)
	(let* ((c0 (car c-set))
	       (t0 (car c0)))
	  ;;(display c0) (newline)
	  (cond
	   ((equal? t0 'eq)
	    (let ((v1 (s1-lookup-equate rho (cadr c0)))
		  (v2 (s1-lookup-equate rho (caddr c0))))
	      (if (equal? v1 v2)
		  (loop rho (cdr c-set) acc)
		  (loop (cons (cons v1 v2) rho) (cdr c-set) acc))))
	   ((equal? t0 'fct)
	    (let* ((v3 (s1-lookup-equate rho (cadddr c0)))
		   (c1 (let loop ((c-set (append (cdr c-set) acc)))
			 (if (null? c-set)
			     #f
			     (let ((c1 (car c-set)))
			       (if (and (equal? 'fct (car c1))
					(equal? v3 (s1-lookup-equate rho (cadddr c1))))
				   c1
				   (loop (cdr c-set))))))))
	      (if c1
		  (let ((v01 (s1-lookup-equate rho (cadr c0)))
			(v02 (s1-lookup-equate rho (caddr c0)))
			(v11 (s1-lookup-equate rho (cadr c1)))
			(v12 (s1-lookup-equate rho (caddr c1))))
		    (loop rho (cons `(eq ,v01 ,v11)
				    (cons `(eq ,v02 ,v12) (cdr c-set))) acc))
		  (loop rho (cdr c-set) (cons c0 acc)))))
	   ((equal? t0 'acons)
	    (let* ((vn (s1-lookup-equate rho (caddr c0)))
		   (c1 (let loop ((c-set (append (cdr c-set) acc)))
			 (if (null? c-set)
			     #f
			     (let ((c1 (car c-set)))
			       (if (and (equal? 'acons (car c1))
					(equal? vn (s1-lookup-equate rho (caddr c1))))
				   c1
				   (loop (cdr c-set))))))))
	      (if c1
		  (loop rho
			(append (map (lambda (v0 v1) `(eq ,v0 ,v1))
				     (cadr c0) (cadr c1))
				(cdr c-set))
			acc)
		  (loop rho (cdr c-set) (cons c0 acc)))))
	   (else
	    (loop rho (cdr c-set) (cons c0 acc)))
	   )))))

;;; 4. make results usable

;;; change procedure call flow equations (peq) into flow equations (eq)
(define (s1-flow-peq->eq CC*)
  (map (lambda (c0)
	 (if (equal? (car c0) 'peq)
	     (cons 'eq (cdr c0))
	     c0))
       CC*))

;;; change constructor flow constraints (acons) into flow equations (eq)
(define (s1-flow-acons->eq CC*)
  (if (null? CC*)
      '()
      (let ((c0 (car CC*)))
	(if (equal? (car c0) 'acons)
	    (append (map (lambda (X) `(eq ,X ,(caddr c0))) (cadr c0))
		    (s1-flow-acons->eq (cdr CC*)))
	    (cons c0 (s1-flow-acons->eq (cdr CC*)))))))

(define (s1-flow-analysis D*)
  (let* ((DCN (s1-flow-constraints D*))
	 (DF* (car DCN))
	 (CC* (s1-flow-peq->eq (cadr DCN))))
    (cons DF* (s1-flow-simplify CC*))))

(define (s1-flow-of x) (car x))

(define (s1-meat-of x) (cdr x))

(define (s1-lookup-equate rho v)
  (if (null? rho) v
      (if (equal? (caar rho) v)
	  (cdar rho)
	  (s1-lookup-equate (cdr rho) v))))

(define (s1-maybe-closure? E CCS)
  (let* ((rho (car CCS))
	 (c-set (cdr CCS))
	 (flow (s1-lookup-equate rho (s1-flow-of E))))
    (let loop ((CCS c-set))
      (if (null? CCS)
	  #f				;definitely no closure
	  (let ((c0 (car CCS)))
	    (if (and (equal? 'fct (car c0))
		     (equal? flow (s1-lookup-equate rho (cadddr c0))))
		#t
		(loop (cdr CCS)))))))) 

(define (s1-maybe-closure?* E* CCS)
  (map (lambda (E) (s1-maybe-closure? E CCS)) E*)) 

;;; is there a literal lambda which may flow together with E?
;;; if so, return a pair (argument-flow . result-flow)
(define (s1-flow-lambda? flow-var CCS D*)
  (let* ((rho (car CCS))
	 (c-set (cdr CCS))
	 (flow-var (s1-lookup-equate rho flow-var)))

    (define (ormap p xs)
      (if (null? xs)
	  #f
	  (or (p (car xs))
	      (ormap p (cdr xs)))))

    (let d-loop ((D* D*))
      (if (null? D*)
	  #f
	  (or (let e-loop ((E0 (s1-get-proc-body (car D*))))
		(let ((E (s1-meat-of E0)))
		  (cond
		   ((or (s1-is-V? E) (s1-is-value? E) (s1-is-quote? E))
		    #f)
		   ((s1-is-if? E)
		    (or (e-loop (s1-get-if-cond E))
			(e-loop (s1-get-if-then E))
			(e-loop (s1-get-if-else E))))
		   ((s1-is-call? E)
		    (ormap e-loop (s1-get-call-args)))
		   ((s1-is-lambda? E)
		    (or (and (equal? (s1-lookup-equate rho (s1-flow-of E0)) flow-var)
			     (cons (s1-flow-of (s1-get-lambda-arg E))
				   (s1-flow-of (s1-get-lambda-body E))))
			(e-loop (s1-get-lambda-body E))))
		   ((s1-is-apply? E)
		    (or (e-loop (s1-get-apply-rator E))
			(e-loop (s1-get-apply-rand E))))
		   (else
		    (ormap e-loop (s1-get-op-args E))))))
	      (d-loop (cdr D*)))))))

;;; returns the distance from the source of dynamicity
;;; distance = nr of flow constraints which have been traversed
(define (s1-maybe-dynamic? flow-var CCS B*)
  (let* ((rho (car CCS))
	 (c-set (cdr CCS))
	 (dynamics
	  (let loop ((B* B*))
	    (if (null? B*)
		'()
		(if (equal? 'D (cdar B*))
		    (cons (s1-lookup-equate rho (caar B*))
			  (loop (cdr B*)))
		    (loop (cdr B*)))))))
    (let loop ((flow-var (s1-lookup-equate rho flow-var))
	       (visited '())
	       (distance 0))
      (cond
       ((member flow-var dynamics)
	distance)
       ((member flow-var visited)
	#f)
       (else
	(let inner-loop ((c-set c-set))
	  (if (null? c-set)
	      #f
	      (let ((c0 (map (lambda (z) (s1-lookup-equate rho z)) (car c-set))))
		(cond
		 ((and (equal? (car c0) 'fct)
			 (or (equal? flow-var (cadr c0))
			     (equal? flow-var (caddr c0))))
		    (or (loop (cadddr c0)
			      (cons flow-var visited)
			      (+ 1 distance))  
			(inner-loop (cdr c-set))))
		 ((and (equal? (car c0) 'dep)
		       (equal? flow-var (caddr c0)))
		  (or (loop (cadr c0)
			    (cons flow-var visited)
			    (+ 1 distance))
		      (inner-loop (cdr c-set))))
		 (else
		  (inner-loop (cdr c-set)))))))))))) 

;;; compute the binding-time type of flow-var from the constraints and
;;; the initial binding-times
(define (s1-binding-time-E* E* CCS B*)
  (begin
    ;;(display "s1-binding-time-E* ") (display E*) (display CCS) (display B*) (newline)
    (map (lambda (E0) (s1-binding-time (s1-flow-of E0) CCS B*)) E*)))
(define (s1-binding-time flow-var CCS B*)
  (let* ((rho (car CCS))
	 (c-set (cdr CCS))
	 (initial-alist
	  (map (lambda (v/bt)
		 (cons (s1-lookup-equate rho (car v/bt))
		       (cdr v/bt))) B*)))
    ;; bt ::= S | D | (-> bt bt)
    ;; start function body
    ;; if flow is dynamic, return 'D
    ;; otherwise investigate function constraints, etc
    (or
     (let loop ((flow-var (s1-lookup-equate rho flow-var))
		(visited '()))
       (let ((initial-bt (assoc flow-var initial-alist)))
	 (cond
	  (initial-bt
	   (cdr initial-bt))
	  ((s1-maybe-dynamic? flow-var CCS B*)
	   'D)
	  ((member flow-var visited)
	   #f)
	  (else
	   (let inner-loop ((c-set c-set))
	     (if (null? c-set)
		 'S
		 (let ((c0 (map (lambda (z) (s1-lookup-equate rho z))
				(car c-set))))
		   (cond
		    ((and (equal? (car c0) 'fct)
			  (equal? flow-var (cadddr c0)))
		     `(-> ,(loop (cadr c0) (cons flow-var visited))
			  ,(loop (caddr c0) (cons flow-var visited))))
		    (else
		     (inner-loop (cdr c-set)))))))))))
    'S))) 

;;; input:  list of pairs (flow . bt)
;;; output: list of pairs (flow . D)
(define (s1-bt-generalize CCS B*)
  (let ((rho (car CCS))
	(c-set (cdr CCS)))
    (define (bt-get-argument bt)
      (if (pair? bt)
	  (list-ref bt 1)
	  bt))
    (define (bt-get-result bt)
      (if (pair? bt)
	  (list-ref bt 2)
	  bt))
    (let loop ((B* B*))
      (if (null? B*)
	  '()
	  (append
	   (let inner-loop ((flow-var (s1-lookup-equate rho (caar B*)))
			    (bt (cdar B*)))
	     (cond
	      ((equal? 'D bt)
	       (list (cons flow-var 'D)))
	      ((and (pair? bt) (equal? (car bt) '->))
	       (let ((c0 (s1-find-fct flow-var CCS)))
		 (if c0
		     (append (inner-loop (s1-lookup-equate rho (cadr c0))
					 (bt-get-argument bt))
			     (inner-loop (s1-lookup-equate rho (caddr c0))
					 (bt-get-result bt)))
		     '())))
	      (else
	       '()))
	     )
	   (loop (cdr B*)))))))

;;; find a function constraint for function 'flow-var'
(define (s1-find-fct flow-var CCS)
  (let ((rho (car CCS))
	(c-set (cdr CCS)))
    (let loop ((c-set c-set))
      (if (null? c-set)
	  #f
	  (let ((c0 (car c-set)))
	    (if (and (equal? (car c0) 'fct)
		     (equal? flow-var (s1-lookup-equate rho (cadddr
							  c0))))
		c0
		(loop (cdr c-set))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cltable.adt

;;;
;;; collect closures and information about them
;;;

;;; D* is a flow annotated list of definitions

(define (s1-cltable D*)
  (let loop ((D* D*))
    (if (null? D*)
	'()
	(let ((D (car D*)))
	  (append (s1-cltable-exp (s1-get-proc-body D))
		  (loop (cdr D*)))))))

;;; E is a flow annotated expression

(define (s1-cltable-exp E)
  (let loop ((E0 E))
    (let ((f (s1-flow-of E0))
	  (E (s1-meat-of E0)))
      (cond
       ((or (not (list? E))
	    (null? E)
	    (equal? 'quote (car E)))
	'())
       ((equal? 'if (car E))
	(append (loop (s1-get-if-cond E))
		(loop (s1-get-if-then E))
		(loop (s1-get-if-else E))))
       ((equal? 'call (car E))
	(apply append (map loop (s1-get-call-args E))))
       ((equal? 'lambda (car E))
	(cons E0
	      (loop (s1-get-lambda-body E))))
       ((equal? 'apply (car E))
	(append (loop (s1-get-apply-rator E))
		(loop (s1-get-apply-rand E))))
       (else
	(apply append (map loop (s1-get-op-args E)))))))) 

;;; access functions

(define (s1-cltable:nr->exp cltable nr)
  (let loop ((alist cltable))
    (if (null? alist)
	'***error***
	(if (equal? (caar alist) nr)
	    (cdar alist)
	    (loop (cdr alist)))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reach.adt
;;; $Header $
;;; $Log $
;;; 
;;;(loadt "scheme1.adt")
;;;(loadt "intset.adt")
;;;
;;; determine the set of applications reachable through an execution
;;; path from every lambda's body 
;;;

;;;
;;; given an annotated expression `e', an alist `reach-lambda', which maps
;;; lambdas to reachable applications so far, and an alist
;;; `reach-body', which maps procedure names to reachable applications
;;; from their bodies, find the applications
;;; reachable from `e'
;;; more precisely: we are interested in the flow of the operator part
;;; of the application, so that's what we collect
;;;

(define (s1-reachable-exp E reach-lambda reach-body)
  (let loop ((E0 E))
    (let ((f (s1-flow-of E0))
	  (E (s1-meat-of E0)))
      (cond
       ((or (not (list? E))
	    (null? E)
	    (equal? 'quote (car E)))
	'())
       ((equal? 'if (car E))
	(s1-union (loop (s1-get-if-cond E))
	       (loop (s1-get-if-then E))
	       (loop (s1-get-if-else E))))
       ((equal? 'call (car E))
	(s1-union (apply s1-union (map loop (s1-get-call-args E)))
	       (cdr (assoc (s1-get-call-proc E) reach-body))))
       ((equal? 'let (car E))
	(s1-union (loop (s1-get-let-bound-expression E))
	       (loop (s1-get-let-expression E))))
       ((equal? 'lambda (car E))
	(loop (s1-get-lambda-body E)))
       ((equal? 'apply (car E))
	(s1-union (list (s1-flow-of (s1-get-apply-rator E)))
	       (loop (s1-get-apply-rator E))
	       (loop (s1-get-apply-rand E))))
       (else
	(apply s1-union (map loop (s1-get-op-args E)))))))) 

;;;
;;; given reach-lambda and reach-body compute a new version of
;;; reach-body (from the list of definitions D*)
;;;

(define (s1-reachable-def D* reach-lambda reach-body)
  (let loop ((D* D*))
    (if (null? D*)
	'()
	(cons (cons (s1-get-proc-name (car D*))
		    (s1-reachable-exp (s1-get-proc-body (car D*))
				   reach-lambda
				   reach-body))
	      (loop (cdr D*))))))

;;;
;;; given reach-lambda and reach-body compute a new version of
;;; reach-lambda (from the list of lambdas I)
;;;

(define (s1-reachable-lambda I reach-lambda reach-body)
  (let loop ((I I))
    (if (null? I)
	'()
	(cons (cons (caar I)
		    (s1-reachable-exp (car I) reach-lambda reach-body))
	      (loop (cdr I))))))

;;;
;;; `s1-reachability' does the fixpoint computation, given the list `D*'
;;; of annotated definitions and `I' of lambdas
;;;

(define (s1-reachability D* I)
  (let loop
      ((reach-lambda (map (lambda (E) (cons (car E) '())) I))
       (reach-body   (map (lambda (D) (cons (s1-get-proc-name D) '())) D*)))
    (let ((new-lambda (s1-reachable-lambda I reach-lambda reach-body))
	  (new-body   (s1-reachable-def  D*  reach-lambda reach-body)))
      (if (and (equal? reach-lambda new-lambda)
	       (equal? reach-body new-body))
	  (cons reach-lambda reach-body)
	  (loop new-lambda new-body)))))

;;; determine `recursive' lambdas
;;; returns the list of lambda-flows that must be dynamic

(define (s1-recursive-lambdas D* I CCS)
  (let* ((rho (car CCS))
	 (c-set (cdr CCS))
	 (reach-lambda (car (s1-reachability D* I))))
    (let loop ((reach-lambda reach-lambda))
      (if (null? reach-lambda)
	  '()
	  (let* ((this-reach (car reach-lambda))
		 (this-flow (s1-lookup-equate rho (car this-reach)))
		 (this-operators (cdr this-reach)))
	    (let inner-loop ((rators this-operators))
	      (if (null? rators)
		  (loop (cdr reach-lambda))
		  (if (equal? this-flow (s1-lookup-equate rho (car
							    rators)))
		      (cons this-flow (loop (cdr reach-lambda)))
		      (inner-loop (cdr rators)))))))))) 

;;; classify a lambda as dynamic if
;;; - it is recursive
;;; - if it might be captured as a free variable in a dynamic lambda
;;; - if a primitive operator is applied to it

(define (s1-classify d-flows cltab ccs)
  (let* ((rho (car ccs)))
    ;; gobble up (new) lambdas that have dynamic flow (according to
    ;; `d-flow')
    ;; output: labels of lambdas
    (define (case1 d-flows d-lambdas)
      (let loop ((d-lambdas d-lambdas) (cltab cltab))
	(if (null? cltab)
	    d-lambdas
	    (let* ((this-lambda (car cltab))
		   (this-flow   (s1-flow-of this-lambda)))
	      (if (member (s1-lookup-equate rho this-flow) d-flows)
		  (loop (s1-union (cons this-flow d-lambdas)) (cdr cltab))
		  (loop d-lambdas (cdr cltab)))))))
    ;; gobble up flows that are captured in dynamic closures
    ;; output: normalized flows (equivalence classes)
    (define (case2 d-flows d-lambdas)
      (let loop ((d-flows d-flows) (d-lambdas d-lambdas))
	(if (null? d-lambdas)
	    d-flows
	    (let ((this-lambda (assoc (car d-lambdas) cltab)))
	      (loop (s1-union d-flows
			   (map (lambda (flow) (s1-lookup-equate rho flow))
				(s1-free-flows this-lambda '())))
		    (cdr d-lambdas))))))
    (let loop ((d-flows d-flows) (d-lambdas '()))
      (let* ((new-lambdas (case1 d-flows d-lambdas))
	     (new-flows   (case2 d-flows new-lambdas)))
	(if (and (equal? new-lambdas d-lambdas)
		 (equal? new-flows d-flows))
	    d-lambdas
	    (loop new-flows new-lambdas))))
)) 


(define (s1-free-flows e0 bounds)
  (let ((e (s1-meat-of e0))
	(f (s1-flow-of e0)))
    (cond
     ((s1-is-v? e)
      (if (member e bounds) '() (list f)))
     ((or (not (list? E))
	      (null? E)
	      (equal? 'quote (car E)))
      '())
     ((equal? 'if (car E))
      (apply s1-union (map (lambda (e) (s1-free-flows e bounds)) (cdr E))))
     ((equal? 'call (car E))
      (apply s1-union (map (lambda (e) (s1-free-flows e bounds)) (cddr E))))
     ((equal? 'let (car E))
      (s1-union (s1-free-flows (s1-get-let-bound-expression E) bounds)
	     (s1-free-flows (s1-get-let-expression E)
			 (cons (s1-meat-of (s1-get-let-variable E)) bounds))))
     ((equal? 'apply (car E))
      (apply s1-union (map (lambda (e) (s1-free-flows e bounds)) (cddr E))))
     ((equal? 'lambda (car E))
      (s1-free-flows (s1-get-lambda-body E)
		  (cons (s1-meat-of (s1-get-lambda-arg E)) bounds)))
     (else
      (apply s1-union (map (lambda (e) (s1-free-flows e bounds)) (cdr E))))))) 

;;; flows to/from primops

(define (s1-primop-flows-def D* ccs)
  (let ((rho (car ccs)))
    (define (s1-primop-flows-exp e0)
      (let loop ((e0 e0))
	(let ((e (s1-meat-of e0))
	      (f (s1-flow-of e0)))
	  (cond
	   ((s1-is-v? e)
	    '())
	   ((or (not (list? E))
		(null? E)
		(equal? 'quote (car E)))
	    '())
	   ((member (car E) '(if apply))
	    (apply s1-union (map loop (cdr E))))
	   ((equal? (car E) 'let)
	    (s1-union (loop (s1-get-let-bound-expression E))
		   (loop (s1-get-let-expression E))))
	   ((equal? (car E) 'call)
	    (apply s1-union (map loop (cddr E))))
	   ((equal? (car E) 'lambda)
	    (loop (s1-get-lambda-body E)))
	   ;; data structures separate flows
	   ((member (car E) '(acons acar acdr anull?))
	    (map loop (cdr E)))
	   (else
	    (apply s1-union (cons (list (s1-lookup-equate rho f))
			       (map loop (cdr E)))))
	   ))))
    (let loop ((D* D*))
      (if (null? D*)
	  '()
	  (s1-union (s1-primop-flows-exp (s1-get-proc-body (car D*)))
		 (loop (cdr D*)))))
    ))

;;; overall procedure

(define (s1-dyn-analysis D* I CCS)
  (let* ((d-flows (s1-recursive-lambdas D* I CCS))
	 (p-flows (s1-primop-flows-def D* CCS))
	 (d-lambdas (s1-classify (s1-union d-flows p-flows) I CCS)))
    d-lambdas)) 

;;; given a program point/flow `F', the substitution `rho', and the
;;; program points `d-lambdas' of all dynamic lambdas
;;; return the set of lambdas that might actually turn up at point F

(define (s1-dyn-functions F CCS d-lambdas)
  (let* ((rho (car CCS))
	 (this-flow (s1-lookup-equate rho F)))
    (let loop ((d-lambdas d-lambdas))
      (if (null? d-lambdas)
	  '()
	  (if (equal? this-flow (s1-lookup-equate rho (car d-lambdas)))
	      (cons (car d-lambdas) (loop (cdr d-lambdas)))
	      (loop (cdr d-lambdas)))))
    )) 

(define (s1-all-functions F CCS cltable)
  (s1-dyn-functions F CCS (map car cltable)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; oca.adt
;;; oca --- occurence count analysis
;;; main use: avoid code duplication by inserting memoization points
;;; for variables of function type which occur more than once. In
;;; principle, we can restart counting inside of every static
;;; conditional. We don't do that and just count globally,
;;; instead. Just to try it out. Also, the other option would be much
;;; more expensive.

;;; purpose: annotate every use of a variable with the number of its
;;; occurrences in the current procedure.
;;; returns an alist of program points (where a variable occurs) and
;;; the number of its occurrences.

(define (scheme1-oca d*)
  (let loop ((d* d*) (r '()))
    (if (null? d*)
	r
	(let* ((D (car d*))
	       (r1 (scheme1-oca-e (s1-get-proc-body D)
			  (map (lambda (v) (cons v (cons '() 0)))
			       (s1-get-proc-args D)))))
	  (loop (cdr d*) (append r1 r))))))

(define (scheme1-oca-e e env)
  ;; structure of env: list of (var label-list . occurence-count)
  ;; the loop is evaluated for its effect, only. Returns #unspecific
  (begin
  (let loop ((e0 e))
    (let ((e (s1-meat-of e0)))
      (cond
       ((s1-is-V? E)
	(let ((info (cdr (assoc E env))))
	  (set-car! info (cons (s1-flow-of e0) (car info)))
	  (set-cdr! info (+ (cdr info) 1))))
       ((s1-is-IF? E)
	(loop (s1-get-if-cond E))
	(loop (s1-get-if-then E))
	(loop (s1-get-if-else E)))
       ((s1-is-LET? E)
	(loop (s1-get-let-bound-expression E))
	;; enter scope
	(set! env (cons (cons (s1-meat-of (s1-get-let-variable E))
			      (cons '() 0)) env))
	(loop (s1-get-let-expression E))
	;; leave scope
	(set! env (append (cdr env) (list (car env)))))
       ((s1-is-CALL? E)
	(map loop (s1-get-call-args E)))
       ((s1-is-LAMBDA? E)
	;; enter scope
	(set! env (cons (cons (s1-meat-of (s1-get-lambda-arg E))
			      (cons '() 0)) env))
	(loop (s1-get-lambda-body E))
	;; leave scope
	(set! env (append (cdr env) (list (car env)))))
       ((s1-is-APPLY? E)
	(loop (s1-get-apply-rator E))
	(loop (s1-get-apply-rand E)))
       )))
  (let ((result '()))
    (map (lambda (var-labels-occ)
	   (let* ((labels-occ (cdr var-labels-occ))
		  (occ (cdr labels-occ)))
	     (map (lambda (label)
		    (set! result
			  (cons (cons label occ) result))) (car labels-occ))))
	 env)
      result))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; intset.adt

(define (s1-union . args)
  (if (null? args)
      '()
      (let loop ((xs (car args))
		 (u (apply s1-union (cdr args))))
	(if (null? xs)
	    u
	    (loop (cdr xs) (s1-insert (car xs) u)))))) 

(define (s1-insert x xs)
  (cond
   ((null? xs)
    (list x))
   ((< x (car xs))
    (cons x xs))
   ((= x (car xs))
    xs)
   (else
    (cons (car xs) (s1-insert x (cdr xs)))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; debug.adt
(define dpy display) 
(define (nl) (begin (newline) #f)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous additions

;;; free variables of expressions
(define (s1-freevars E)
  (let ((E (s1-meat-of E)))
  (if (s1-is-V? E)
      (list E)
      (if (or (not (list? E))
	      (null? E)
	      (equal? 'quote (car E)))
	  '()
	  (if (equal? 'if (car E))
	      (s1-freevars* (cdr E))
	      (if (equal? 'call (car E))
		  (s1-freevars* (cddr E))
		  (if (equal? 'apply (car E))
		      (s1-freevars* (cdr E))
		      (if (equal? 'lambda (car E))
			  (s1-set-subtract (s1-freevars (s1-get-lambda-body E))
					(s1-meat-of (s1-get-lambda-arg E)))
			  (s1-freevars* (cdr E))))))))))
(define (s1-freevars* E*)
  (if (null? E*) '()
      (s1-set-union (s1-freevars (car E*))
		 (s1-freevars* (cdr E*)))))

(define (s1-set-union s1 s2)
  (if (null? s1) s2
      (if (member (car s1) s2)
	  (s1-set-union (cdr s1) s2)
	  (cons (car s1) (s1-set-union (cdr s1) s2)))))

(define (s1-set-subtract s e)
  (if (null? s)
      '()
      (if (equal? e (car s))
	  (cdr s)
	  (cons (car s) (s1-set-subtract (cdr s) e)))))

