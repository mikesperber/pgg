;;; info record attached to
;;; let x = e1 in e2   and
;;; unit e
(define-record info
  (astore				;per-program-point store
   varcardmap				;per-program-point variable cardinality map
   refcardmap				;per-program-point reference cardinality map
   init					;per-program-point general reachability
   ))

(define-record aval
  (pps)					;vector of program points
  (neighbors '()))

;;; each field holds a vector
;;; entry i of a vector is either
;;;  #f, meaning that value #i is present, or
;;;  a list of constraints to be enforced when #i becomes available
(define (present? x)
  (not x))
(define present #f)
(define not-present '())

(define (new-init)
  (make-vector 1 not-present))

(define (new-aval)
  (make-aval (make-vector *anf-pp-count* not-present)))
(define (new-top-aval)
  (make-aval (make-vector *anf-pp-count* present)))
(define (aval-freevarsof aval)
  (let ((vec (aval->pps aval)))
    (let loop ((i 0) (result empty-labset))
      (if (< i *anf-pp-count*)
	  (let ((obj (vector-ref *anf-pp-map* i)))
	    (if (and (anf-lambda? obj)
		     (present? (vector-ref vec i)))
		(let ((fvs (anf-lambda->free obj)))
		  (loop (+ i 1) (labset-union fvs result)))
		(loop (+ i 1) result)))
	  result))))
(define (aval-refsof aval)
  ;; returns refs and vectors
  (let ((vec (aval->pps aval)))
    (let loop ((i 0) (result empty-labset))
      (if (< i *anf-pp-count*)
	  (let ((obj (vector-ref *anf-pp-map* i)))
	    (if (and (or (anf-ref? obj) (anf-vector? obj))
		     (present? (vector-ref vec i)))
		(loop (+ i 1) (labset-add i result))
		(loop (+ i 1) result)))
	  result))))
(define (aval-display aval)
  (display "{")
  (let ((vec (aval->pps aval)))
    (let loop ((i 0))
      (if (< i *anf-pp-count*)
	  (begin
	    (if (present? (vector-ref vec i))
		(begin
		  (display " ")
		  (display i)))
	    (loop (+ i 1))))))
  (display "}"))
(define (latex-display-aval aval)
  ;; assume math mode
  (display "\\{")
  (let ((vec (aval->pps aval)))
    (let loop ((i 0))
      (if (< i *anf-pp-count*)
	  (begin
	    (if (present? (vector-ref vec i))
		(begin
		  (display i)
		  (display ",")))
	    (loop (+ i 1))))))
  (display "\\}"))

(define-record astore
  (vec)
  (neighbors '())
  (dom '()))

(define (new-astore)			;might be too large
  (let ((vec (make-vector *anf-pp-count* #f)))
;;;    (let loop ((i 0))
;;;      (if (< i *anf-pp-count*)
;;;	  (begin
;;;	    (if (anf-ref? (vector-ref *anf-pp-map* i))
;;;		(vector-set! vec i (new-aval)))
;;;	    (loop (+ i 1)))))
    (make-astore vec)))
(define (astore-ref astore i)
  (let* ((vec (astore->vec astore))
	 (entry (vector-ref vec i)))
    (if entry
	entry
	(let ((aval (new-aval)))
	  (astore-set! astore i aval)
	  aval))))
(define (astore-set! astore i x)
  (let* ((vec (astore->vec astore))
	 (entry (vector-ref vec i)))
    (if (not entry)
	(astore->dom! astore (cons i (astore->dom astore))))
    (vector-set! vec i x)
    (for-each (lambda (proc) (proc i x)) (astore->neighbors astore))))
(define (astore-length astore)
  (vector-length (astore->vec astore)))
(define (astore-walk astore proc)
  (let ((vec (astore->vec astore)))
    (for-each (lambda (i) (proc i (vector-ref vec i)))
	      (astore->dom astore))))
(define (astore-add-neighbor astore proc)
  (astore->neighbors! astore (cons proc (astore->neighbors astore))))

(define anf-get-astore
  (lambda (anf)
    (anf-get-something anf info->astore)))
(define anf-get-varcardmap
  (lambda (anf)
    (anf-get-something anf info->varcardmap)))
(define anf-get-refcardmap
  (lambda (anf)
    (anf-get-something anf info->refcardmap)))
(define anf-get-init
  (lambda (anf)
    (anf-get-something anf info->init)))
(define anf-get-something
  (lambda (anf accessor)
    (cond
     ((anf-let? anf)
      (accessor (anf-let->info anf)))
     ((anf-unit? anf)
      (accessor (anf-unit->info anf)))
     ((anf-cond? anf)
      (accessor (anf-cond->info anf)))
     ((anf-app? anf)
      (accessor (anf-app->info anf)))
     (else
      (error "no info available")))))

(define (anf-get-nr anf)
  (cond
   ((anf-ref? anf)
    (anf-ref->nr anf))
   ((anf-vector? anf)
    (anf-vector->nr anf))
   (else
    (error "anf-get-nr: ref or vectro expected"))))

(define-record cardmap
  (fm)
  (neighbors '()))
(define (new-varcardmap)
  (make-cardmap (make-symbol-fm '())))
(define (new-refcardmap)
  (make-cardmap (make-number-fm '())))
(define (cardmap-lookup cm key)
  (let ((res (fm-lookup (cardmap->fm cm) key)))
    (and res (cdr res))))
(define (cardmap-update! cm key value)
  (fm-update! (cardmap->fm cm) key value))
(define (cardmap-walk cm proc)
  (fm-walk (cardmap->fm cm) proc))
(define (cardmap-prime cm w)
  (if (cardmap-lookup cm w)
      'nothing-to-do
      (cardmap-update! cm w '())))
(define (latex-display-cardmap cm)
  (cardmap-walk cm
		(lambda (key card)
		  (if (cardinality? card)
		      (begin
			(if (symbol? key)
			    (latex-display-symbol key)
			    (display key))
			(display " $\\mapsto$ ")
			(latex-display-symbol card)
			(display ", ")))))
  (newline))
(define (cardmap-display cm)
  (cardmap-walk cm
		(lambda (key card)
		  (if (cardinality? card)
		      (begin
			(display key)
			(display " -> ")
			(display card)
			(display ", "))))))

(define cardinality? symbol?)
(define (cardinality<=? c1 c2)
  (or (eq? c1 'single) (eq? c2 'multiple)))
(define cardinality-single 'single)
(define cardinality-multiple 'multiple)
(define cardinality-join
  (lambda c*
    (if (null? c*)
	#f
	(let ((c1 (car c*))
	      (c2 (apply cardinality-join (cdr c*))))
	  (if (cardinality? c1)
	      (if (cardinality? c2)
		  cardinality-multiple
		  c1)
	      c2)))))
(define cardinality-lub
  (lambda c*
    (if (null? c*)
	#f
	(let ((c1 (car c*))
	      (c2 (apply cardinality-lub (cdr c*))))
	  (if (cardinality? c1)
	      (if (cardinality? c2)
		  (if (cardinality<=? c1 c2)
		      c2
		      c1)
		  c1)
	      c2)))))

(define (new-flowmap)
  (make-symbol-fm '()))
(define flowmap-update! fm-update!)
(define (flowmap-lookup flowmap x)
  (let ((res (fm-lookup flowmap x)))
    (and res (cdr res))))

(define (latex-display-flowmap flowmap)
  (display "\\begin{flushleft}") (newline)
  (display "AEnv~\\begin{tabular}[t]{lcl}") (newline)
  (fm-walk flowmap
	   (lambda (sym aval)
	     (latex-display-symbol sym)
	     (display "& $\\mapsto$& $")
	     (latex-display-aval aval)
	     (display "$\\\\")))
  (display "\\end{tabular}") (newline)
  (display "\\end{flushleft}") (newline)
  (newline))
(define (flowmap-display flowmap)
  (display "FLOWMAP BEGIN") (newline)
  (fm-walk flowmap
	   (lambda (sym aval)
	     (display sym)
	     (display " -> ")
	     (aval-display aval)
	     (newline)))
  (display "FLOWMAP END") (newline)
  (newline))

;;; loop over all program points
(define (pp-loop! select? proc)
  (let loop ((i 0))
    (if (< i *anf-pp-count*)
	(let ((obj (vector-ref *anf-pp-map* i)))
	  (if (select? obj)
	      (proc i obj))
	  (loop (+ i 1))))))

(define (anf-ref-loop! proc)
  (for-each (lambda (obj) (proc (anf-ref->nr obj) obj)) *anf-refs*))

(define (anf-vector-loop! proc)
  (for-each (lambda (obj) (proc (anf-vector->nr obj) obj)) *anf-vectors*))

(define (anf-lambda-loop! proc)
  (for-each (lambda (obj) (proc (anf-lambda->nr obj) obj)) *anf-lambdas*))

(define (anf-ctor-loop! proc)
  (for-each (lambda (obj) (proc (anf-ctor->nr obj) obj)) *anf-ctors*))

(define (pp-loop-acc select? init proc)
  (let loop ((i 0) (result init))
    (if (< i *anf-pp-count*)
	(let ((obj (vector-ref *anf-pp-map* i)))
	  (if (select? obj)
	      (loop (+ i 1) (proc i obj result))
	      (loop (+ i 1) result)))
	result)))

;;; defn constraint: InitFS
(define (initialize! init-vec)
  (let ((entry (vector-ref init-vec 0)))
    (if (present? entry)
	'nothing-to-do
	(begin
	  (vector-set! init-vec 0 present)
	  (process-constraints entry)))))

;;; defn constraint: if InitFS then thunk
(define (if-init-thunk! init-vec thunk)
  (let ((entry (vector-ref init-vec 0)))
    (if (present? entry)
	(thunk)
	(vector-set! init-vec 0 (cons thunk entry)))))

;;; defn constraint: i \in aval
(define (add-singleton! i aval)
  (let* ((vec (aval->pps aval))
	 (entry (vector-ref vec i)))
    (if (not (present? entry))
	(begin
	  (vector-set! vec i present)
	  (process-constraints entry)
	  (for-each (lambda (aval) (add-singleton! i aval))
		    (aval->neighbors aval))))))

;;; defn constraint: aval1 <= aval2
(define (add-subset! aval1 aval2)
  (aval->neighbors! aval1 (cons aval2 (aval->neighbors aval1)))
  (let* ((vec (aval->pps aval1)))
    (let loop ((i 0))
      (if (< i *anf-pp-count*)
	  (begin
	    (if (present? (vector-ref vec i))
		(add-singleton! i aval2))
	    (loop (+ i 1)))))))

;;; constraint: if i \in aval1 then aval2 <= aval3
(define (add-conditional! i aval1 aval2 aval3)
  (add-conditional!-internal i aval1 (lambda ()
				       ;;(display "!!! point 1") (newline)
				       (add-subset! aval2 aval3))))
;;; if aval2*and aval3* are lists of aval
(define (add-conditional*! i aval1 aval2* aval3*)
  (add-conditional!-internal i aval1 (lambda ()
				       ;;(display "!!! point 2") (newline)
				       (for-each add-subset! aval2* aval3*))))

(define (add-conditional!-internal i aval1 thunk!)
  (let* ((vec (aval->pps aval1))
	 (entry (vector-ref vec i)))
    (if (present? entry)
	(thunk!)
	(vector-set! vec i (cons thunk! entry)))))

(define (process-constraints entry)
  (for-each (lambda (thunk) (thunk)) entry)) ; alternative: (for-each apply entry)

;;; allocate abstract values
(define (allocate-avals-single-astore d* flowmap)
  (let ((astore (new-astore)))
    (allocate-avals-d* d* flowmap (lambda () astore)))) ;only one astore for all

(define (allocate-avals-multiple-astore d* flowmap)
  (allocate-avals-d* d* flowmap new-astore))

(define (allocate-avals-d* d* flowmap new-astore)
  (let* ((new-info
	  (lambda ()
	    (make-info (new-astore)
		       (new-varcardmap)
		       (new-refcardmap)
		       (new-init))))
	 (allocate-avals-e
	  (lambda (anf)
	    (let loop ((anf anf))
	      (cond
	       ((anf-let? anf)
		(flowmap-update! flowmap (anf-let->formal anf) (new-aval))
		(let ((header (anf-let->header anf))
		      (info (new-info)))
		  (anf-let->info! anf info)
		  (cond
		   ((anf-app? header)
		    (anf-app->info! header info))
		   ((anf-cond? header)
		    (anf-cond->info! header info)
		    (loop (anf-cond->then header))
		    (loop (anf-cond->else header)))
		   ((anf-lambda? header)
		    (for-each (lambda (formal) (flowmap-update! flowmap formal (new-aval)))
			      (anf-lambda->formals header))
		    (loop (anf-lambda->body header)))
		   (else
		    'nothing-to-do)))
		(loop (anf-let->body anf)))
	       ((anf-unit? anf)
		(anf-unit->info! anf (new-info)))
	       ((anf-cond? anf)
		(anf-cond->info! anf (new-info))
		(loop (anf-cond->then anf))
		(loop (anf-cond->else anf)))
	       (else
		'nothing-to-do)))))

	(allocate-avals-d
	 (lambda (d)
	   (flowmap-update! flowmap (anf-def->name d) (new-aval))
	   (allocate-avals-e (anf-def->body d)))))
  
    (for-each allocate-avals-d d*)))

;;; allocation of the initial (top) reachmap
(define (allocate-pass-return! anf-app-map)
  (let ((l (vector-length anf-app-map)))
    (let loop ((i 0))
      (if (< i l)
	  (let ((anf-app (vector-ref anf-app-map i)))
	    (anf-app->rpass! anf-app (new-top-reachmap))
	    (anf-app->rreturn! anf-app (new-top-reachmap))
	    (loop (+ i 1)))
	  'finished))))

(define (save-reachmap! anf-app-map)
  (let ((l (vector-length anf-app-map)))
    (let loop ((i 0))
      (if (< i l)
	  (let ((anf-app (vector-ref anf-app-map i)))
	    (anf-app->saved-rpass! anf-app (copy-reachmap (anf-app->rpass anf-app)))
	    (anf-app->saved-rreturn! anf-app (copy-reachmap (anf-app->rreturn anf-app)))
	    (loop (+ i 1)))
	  'finished))))

(define (equal-reachmap? anf-app-map)
  (let ((l (vector-length anf-app-map)))
    (let loop ((i 0))
      (if (< i l)
	  (let ((anf-app (vector-ref anf-app-map i)))
	    (and (aval-vector-equal? (anf-app->saved-rpass anf-app) (anf-app->rpass anf-app))
		 (aval-vector-equal? (anf-app->saved-rreturn anf-app) (anf-app->rreturn anf-app))
		 (loop (+ i 1))))
	  #t))))

(define (aval-vector-equal? aval-vec1 aval-vec2)
  (let ((l1 (vector-length aval-vec1))
	(l2 (vector-length aval-vec2)))
    (and (= l1 l2)
	 (let loop ((i 0))
	   (if (< i l1)
	       (and (aval-equal? (vector-ref aval-vec1 i) (vector-ref aval-vec2 i))
		    (loop (+ i 1)))
	       #t)))))

(define (aval-equal? aval1 aval2)
  (let ((vec1 (aval->pps aval1))
	(vec2 (aval->pps aval2)))
    (let loop ((i 0))
      (if (< i *anf-pp-count*)
	  (let ((entry1 (vector-ref vec1 i))
		(entry2 (vector-ref vec2 i)))
	    (and (or (and (present? entry1) (present? entry2))
		     (and (not (present? entry1)) (not (present? entry2))))
		 (loop (+ i 1))))
	  #t))))

(define (new-top-reachmap)
  (make-vector *anf-pp-count* (new-top-aval)))

(define (copy-reachmap reachmap)
  (do ((vec (make-vector *anf-pp-count*))
       (i 0 (+ i 1)))
      ((>= i *anf-pp-count*) vec)
    (vector-set! vec i (vector-ref reachmap i))))

(define (reachmap-display anf-app-map)
  (let ((l (vector-length anf-app-map)))
    (let loop ((i 0))
      (if (< i l)
	  (let* ((anf-app (vector-ref anf-app-map i))
		 (rpass (anf-app->rpass anf-app))
		 (rreturn (anf-app->rreturn anf-app)))
	    (display "ReachMap [") (anf-display-e anf-app) (display "]") (newline)
	    (let rec ((j 0))
	      (if (< j *anf-pp-count*)
		  (begin
		    (display "  pass[") (display j) (display "] = ")
		    (aval-display (vector-ref rpass j))
		    (newline)
		    (display "  return[") (display j) (display "] = ")
		    (aval-display (vector-ref rreturn j))
		    (newline)
		    (rec (+ j 1)))))
	    (loop (+ i 1)))
	  'finished)))
  (newline))

(define (latex-display-reachmap anf-app-map)
  (display "\\begin{flushleft}") (newline)
  (display "\\begin{tabular}{l@{}l}") (newline)
  (let ((l (vector-length anf-app-map)))
    (let loop ((i 0))
      (if (< i l)
	  (let* ((anf-app (vector-ref anf-app-map i))
		 (rpass (anf-app->rpass anf-app))
		 (rreturn (anf-app->rreturn anf-app)))
	    (display "ReachMap [") (latex-display-anf-e anf-app)
	    (display "] & \\begin{tabular}[t]{lcl}") (newline)
	    (let rec ((j 0))
	      (if (< j *anf-pp-count*)
		  (begin
		    (display "pass[") (display j) (display "]& =& $")
		    (latex-display-aval (vector-ref rpass j))
		    (display "$\\\\")
		    (newline)
		    (display "return[") (display j) (display "]& =&$ ")
		    (latex-display-aval (vector-ref rreturn j))
		    (display "$\\\\")
		    (newline)
		    (rec (+ j 1)))))
	    (display "\\end{tabular}\\\\") (newline)
	    (loop (+ i 1)))
	  'finished)))
  (display "\\end{tabular}")
  (display "\\end{flushleft}")
  (newline))

;;; environment constraints
(define (environment-constraints-d* def* flowmap)
  (for-each (lambda (d) (environment-constraints-d d flowmap)) def*))

(define (environment-constraints-d def flowmap)
  (initialize! (anf-get-init (anf-def->body def)))
  (environment-constraints-e (anf-def->body def) flowmap)
  (let ((def-aval (flowmap-lookup flowmap (anf-def->name def))))
    (for-each (lambda (anf-unit)
		(if-init-thunk!
		 (anf-get-init anf-unit)
		 (lambda ()
		   ;;(display-line "!!! point 3 name=" (anf-def->name def))
		   (add-subset! (flowmap-lookup flowmap
						(anf-var->name (anf-unit->body anf-unit)))
				def-aval))))
	      (anf-def->last def))))

(define (environment-constraints-e anf flowmap)
  (let* ((flowmap-lookup (lambda (key) (flowmap-lookup flowmap key)))
	 (anfvar-lookup (lambda (anfvar) (flowmap-lookup (anf-var->name anfvar)))))
    (let loop ((anf anf))
      (cond
       ((anf-let? anf)
	(let* ((header (anf-let->header anf))
	       (formal (anf-let->formal anf))
	       (formal-aval (flowmap-lookup formal))
	       (body (anf-let->body anf))
	       (init (anf-get-init anf))
	       (init-body (anf-get-init body)))
	  (cond
	   ((anf-var? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       ;;(display "!!! point 4") (newline)
	       (add-subset! (anfvar-lookup header) formal-aval))))
	   ((anf-const? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body))))
	   ((anf-op? header)		;assumes that neither
	    (if-init-thunk!		;functions nor references are processed by primitives  
	     init
	     (lambda ()
	       (initialize! init-body))))
	   ((anf-cond? header)
	    (let ((anf-then (anf-cond->then header))
		  (anf-else (anf-cond->else header)))
	      (if-init-thunk!
	       init
	       (lambda ()
		 (initialize! (anf-get-init anf-then))
		 (initialize! (anf-get-init anf-else))
		 (for-each (lambda (anf-unit)
			     (let ((anfvar (anf-unit->body anf-unit)))
			       ;;(display "!!! point 5") (newline)
			       (add-subset! (anfvar-lookup anfvar) formal-aval)
			       (if-init-thunk!
				(anf-get-init anf-unit)
				(lambda ()
				  (initialize! init-body)))))
			   (append (anf-cond->last-then header)
				   (anf-cond->last-else header)))))
	      (loop anf-then)
	      (loop anf-else)))
	   ((anf-ctor? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       (add-singleton! (anf-ctor->nr header) formal-aval))))
	   ((anf-sel? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       (let ((j (- (anf-sel->field header) 1))
		     (actual-aval (anfvar-lookup (anf-sel->actual header))))
		 (anf-ctor-loop!
		  (lambda (i obj)
		    (add-conditional! i actual-aval
				      (anfvar-lookup
				       (list-ref (anf-ctor->actuals obj) j))
				      formal-aval)))))))
	   ((anf-lambda? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       (add-singleton! (anf-lambda->nr header) formal-aval)))
	    (let ((lambda-body (anf-lambda->body header)))
	      ;; assume that every lambda gets called
	      (initialize! (anf-get-init lambda-body))
	      (loop lambda-body)))
	   ((anf-app? header)
	    (let* ((rator-aval (anfvar-lookup (anf-app->rator header)))
		   (rands-avals (map anfvar-lookup (anf-app->rands header)))
		   (nr-rands (length rands-avals)))
	      (if-init-thunk!
	       init
	       (lambda ()
		 (anf-lambda-loop!
		  (lambda (i obj)
		    (let ((formals (anf-lambda->formals obj)))
		      (if (= nr-rands (length formals))
			  (add-conditional!-internal
			   i rator-aval 
			   (lambda ()
			     ;;(display "!!! point 6, args = ") (display (anf-app->rator header)) (display (anf-app->rands header)) (newline)
			     (for-each add-subset!
				       rands-avals
				       (map flowmap-lookup formals))
			     (for-each (lambda (anf-unit)
					 (let ((last-aval (anfvar-lookup (anf-unit->body anf-unit))))
					   (if-init-thunk!
					    (anf-get-init anf-unit)
					    (lambda ()
					      (initialize! init-body)
					      ;;(display "!!! point 7") (newline)
					      (add-subset! last-aval formal-aval)))))
				       (anf-lambda->last obj))))))))))))
	   ((anf-ref? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       (let* ((nr (anf-ref->nr header))
		      (anfvar (anf-ref->actual header))
		      (aval-actual (anfvar-lookup anfvar))
		      (aval-store (astore-ref (anf-get-astore body)
					      (anf-ref->nr header))))
		 (add-singleton! nr formal-aval)
		 ;;(display "!!! point 8") (newline)
		 (add-subset! aval-actual aval-store)))))
	   ((anf-deref? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       (let ((actual-aval (anfvar-lookup (anf-deref->actual header)))
		     (astore (anf-get-astore anf)))
		 (anf-ref-loop!
		  (lambda (i obj)
		    (add-conditional! i actual-aval
				      (astore-ref astore i)
				      formal-aval)))))))
	   ((anf-assign? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       (let ((ref-aval (anfvar-lookup (anf-assign->ref header)))
		     (actual-aval (anfvar-lookup (anf-assign->actual header)))
		     (astore (anf-get-astore body)))
		 (anf-ref-loop!
		  (lambda (i obj)
		    (add-conditional! i ref-aval
				      actual-aval
				      (astore-ref astore i))))))))
	   ;; vectors
	   ((anf-vector? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       (let* ((nr (anf-vector->nr header))
		      (anfvars (anf-vector->actuals header))
		      (aval-store (astore-ref (anf-get-astore body)
					      (anf-vector->nr header))))
		 (add-singleton! nr formal-aval)
		 (if (anf-vector->kind header)
		     (if (= (length anfvars) 2)
			 (begin
			   ;;(display "!!! point 8 make-vector") (newline)
			   (add-subset! (anfvar-lookup (cadr anfvars)) aval-store)))
		     (for-each (lambda (anfvar)
				 ;;(display "!!! point 8 vector") (newline)
				 (add-subset! (anfvar-lookup anfvar) aval-store))
			       anfvars))))))
	   ((anf-vector-ref? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       (let ((actual-aval (anfvar-lookup (anf-vector-ref->vec header)))
		     (astore (anf-get-astore anf)))
		 (anf-vector-loop!
		  (lambda (i obj)
		    (add-conditional! i actual-aval
				      (astore-ref astore i)
				      formal-aval)))))))
	   ((anf-vector-set? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (initialize! init-body)
	       (let ((ref-aval (anfvar-lookup (anf-vector-set->vec header)))
		     (actual-aval (anfvar-lookup (anf-vector-set->actual header)))
		     (astore (anf-get-astore body)))
		 (anf-vector-loop!
		  (lambda (i obj)
		    (add-conditional! i ref-aval
				      actual-aval
				      (astore-ref astore i))))))))
	   ((anf-call? header)		;have been eliminated in favor of APP
	    (error "anf-call: this should not happen"))	;this has some expense, but is much simpler
	   ((anf-celleq? header)	;eliminated in favor of OP
	    (error "anf-celleq?: this should not happen"))
	   (else
	    (error "not supported" anf header)))
	  (loop body)))
       ((anf-unit? anf)
	'nothing-to-do)
       (else
	(error "anf syntax error" anf))))))

;;;must-alias constraints
;;;constraint cm1 |><| [x -> single] <= cm2
(define (cm-join-single! cm1 x cm2)
  (cm-join-single*! cm1 (list x) cm2))

;;;constraint cm1 |><| [x1, ..., xn -> single] <= cm2
(define (cm-join-single*! cm1 x* cm2)
  (let loop ((x* x*))
    (if (null? x*)
	'finished
	(let ((x (car x*))
	      (x* (cdr x*)))
	  (let ((card (cardmap-lookup cm1 x))
		(doit (lambda () (cm-set-multiple! x cm2))))
	    (if card
		(if (cardinality? card)
		    (doit)
		    (cardmap-update! cm1 x (cons doit card)))
		(begin
		  (cardmap-update! cm1 x (list doit))
		  (cm-set-single! x cm2))))
	  (loop x*))))
  (cm-complement-restricted-leq! cm1 x* cm2))

;;; [ x -> single ] <= cm
(define (cm-set-single! x cm)
  (cm-set! x cm cardinality-single))

;;; [ x -> multiple ] <= cm
(define (cm-set-multiple! x cm)
  (cm-set! x cm cardinality-multiple))

;;; [ x -> new-cardinality ] <= cm
(define (cm-set! x cm new-cardinality)
  (let ((card (cardmap-lookup cm x)))
    (if card
	(if (cardinality? card)
	    (if (cardinality<=? new-cardinality card)
		'nothing-to-do
		(cardmap-enhanced-update! cm x new-cardinality))
	    (begin
	      (cardmap-enhanced-update! cm x new-cardinality)
	      (process-constraints card)))
	(cardmap-enhanced-update! cm x new-cardinality))))

;;; this updater enforces the leq relations to neighbors
(define (cardmap-enhanced-update! cm x new-cardinality)
  (cardmap-update! cm x new-cardinality)
  (for-each (lambda (worker) (worker x new-cardinality))
	    (cardmap->neighbors cm)))

;;; cm1 <= cm2
(define (cm-leq! cm1 cm2)
  (cm-complement-restricted-leq! cm1 '() cm2))

;;; cm1 | Complement vars <= cm2
(define (cm-complement-restricted-leq! cm1 vars cm2)
  (cm-predicate-restricted-leq! cm1 (lambda (var) (not (memq var vars))) cm2))

;;; cm1 | vars <= cm2
(define (cm-restricted-leq! cm1 vars cm2)
  (cm-predicate-restricted-leq! cm1 (lambda (var) (memq var vars)) cm2))

;;; cm1 | pred <= cm2
(define (cm-predicate-restricted-leq! cm1 pred cm2)
  (let ((worker (lambda (var cm1-card)
		  (if (pred var)
		      (if (cardinality? cm1-card)
			  (cm-set! var cm2 cm1-card))))))
    (cardmap->neighbors! cm1 (cons worker (cardmap->neighbors cm1)))
    (cardmap-walk cm1 worker)))

;;; cm1 | vars |><| [ w1...wn -> single ] <= cm2
(define (cm-restricted-join-leq! cm1 vars w* cm2)
  (let ((worker (lambda (var cm1-card)
		  (if (and (memq var vars) (cardinality? cm1-card))
		      (if (memq var w*)
			  (cm-set-multiple! var cm2)
			  (cm-set! var cm2 cm1-card))
		      (if (memq var w*)
			  (cm-set-single! var cm2)
			  'nothing-to-do)))))
    (cardmap->neighbors! cm1 (cons worker (cardmap->neighbors cm1)))
    ;; force execution of the workers for w*
    (for-each (lambda (w) (cardmap-prime cm1 w)) w*)
    (cardmap-walk cm1 worker)))

;;; cm1 | Complement vars1 |><| cm2* | vars2 |><| [ x -> single ] <= cm3
(define (cm-restricted-double-join! cm1 vars1 cm2* vars2 x cm3)
  (let ((worker1 (lambda (var cm1-card)
		   (if (memq var vars1)
		       'nothing-to-do
		       (let ((new-cardinality
			      (cardinality-join cm1-card
						(if (memq var vars2)
						    (apply cardinality-lub
							   (map (lambda (cm2)
								  (cardmap-lookup cm2 var))
								cm2*))
						    #f)
						(if (eq? var x)
						    cardinality-single
						    #f))))
			 (if (cardinality? new-cardinality)
			     (cm-set! var cm3 new-cardinality))))))
	(worker2 (lambda (var cm2-card)
		   (if (memq var vars2)
		       (let ((new-cardinality
			      (cardinality-join cm2-card
						(if (memq var vars1)
						    #f
						    (cardmap-lookup cm1 var))
						(if (eq? var x)
						    cardinality-single
						    #f))))
			 (if (cardinality? new-cardinality)
			     (cm-set! var cm3 new-cardinality))))))
	(workerx (lambda ()
		   (let ((new-cardinality
			  (cardinality-join (if (memq x vars1)
						#f
						(cardmap-lookup cm1 x))
					    (if (memq x vars2)
						(apply cardinality-lub
						       (map (lambda (cm2)
							      (cardmap-lookup cm2 x))
							    cm2*))
						#f)
					    cardinality-single)))
		     (cm-set! x cm3 new-cardinality)))))
    (cardmap->neighbors! cm1 (cons worker1 (cardmap->neighbors cm1)))
    (for-each (lambda (cm2) (cardmap->neighbors! cm2 (cons worker2 (cardmap->neighbors cm2))))
	      cm2*)
    (if x (workerx))
    (cardmap-walk cm1 worker1)
    (for-each (lambda (cm2) (cardmap-walk cm2 worker2)) cm2*)))


;;; must-alias constraint generation
(define (must-alias-constraints-d* def* flowmap)
  (for-each (lambda (d) (must-alias-constraints-d d flowmap)) def*))

(define (must-alias-constraints-d def flowmap)
  (must-alias-constraints-e (anf-def->body def) flowmap))

(define (must-alias-constraints-e anf flowmap)
  (let* ((flowmap-lookup (lambda (key) (flowmap-lookup flowmap key)))
	 (anfvar-lookup (lambda (anfvar) (flowmap-lookup (anf-var->name anfvar)))))
    (let loop ((anf anf))
      (cond
       ((anf-let? anf)
	(let* ((header (anf-let->header anf))
	       (formal (anf-let->formal anf))
	       (body (anf-let->body anf))
	       (vcm-before (anf-get-varcardmap anf))
	       (vcm-after (anf-get-varcardmap body))
	       (init (anf-get-init anf)))
	  (cond
	   ((anf-var? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-const? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-op? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-cond? header)
	    (let ((anf-then (anf-cond->then header))
		  (anf-else (anf-cond->else header)))
	      (if-init-thunk!
	       init
	       (lambda ()
		 (cm-leq! vcm-before (anf-get-varcardmap anf-then))
		 (cm-leq! vcm-before (anf-get-varcardmap anf-else))
		 (for-each (lambda (last-anf) (cm-join-single! (anf-get-varcardmap last-anf)
							       formal
							       vcm-after))
			   (anf-cond->last-then header))
		 (for-each (lambda (last-anf) (cm-join-single! (anf-get-varcardmap last-anf)
							       formal
							       vcm-after))
			   (anf-cond->last-else header))))
	      (loop anf-then)
	      (loop anf-else)))
	   ((anf-call? header)		;have been eliminated in favor of APP
	    'handled-differently)	;this has some expense, but is much simpler
	   ((anf-ctor? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-sel? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-lambda? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after)))
	    (loop (anf-lambda->body header)))
	   ((anf-app? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (let ((rator-aval (anfvar-lookup (anf-app->rator header)))
		     (rpass (anf-app->rpass header))
		     (rreturn (anf-app->rreturn header)))
		 (anf-lambda-loop!
		  (lambda (i obj)
		    (let ((thunk
			   (lambda ()
			     (let* ((vp (aval-freevarsof (vector-ref rpass i)))
				    (vr (aval-freevarsof (vector-ref rreturn i)))
				    (formals (anf-lambda->formals obj))
				    (body (anf-lambda->body obj))
				    (vcm-before-body (anf-get-varcardmap body))
				    (last* (anf-lambda->last obj))
				    (vcm-after-body* (map anf-get-varcardmap last*)))
			       (cm-restricted-join-leq! vcm-before vp
							formals
							vcm-before-body)
			       (for-each
				(lambda (anf-unit vcm-after-body)
				  (if-init-thunk!
				   (anf-get-init anf-unit)
				   (lambda ()
				     (cm-restricted-double-join! vcm-before vp
								 (list vcm-after-body) vr
								 formal
								 vcm-after))))
				last*
				vcm-after-body*)))))
		      (add-conditional!-internal i rator-aval thunk))))))))
	   ((anf-ref? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-deref? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-assign? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ;; vectors
	   ((anf-vector? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-vector-ref? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-vector-set? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   ((anf-celleq? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (cm-join-single! vcm-before formal vcm-after))))
	   (else
	    (error "not supported" anf))))
	(loop (anf-let->body anf)))
       ((anf-unit? anf)
	'nothing-to-do)
       (else
	(error "anf syntax error" anf))))))

;;; abstract garbage collection
;;; updates rpass and rreturn at the same time
(define (collect! anf-app-map flowmap)
  (let ((l (vector-length anf-app-map))
	(aval-bottom (new-aval)))
    (let loop ((i 0))
      (if (< i l)
	  (let* ((anf-app (vector-ref anf-app-map i))
		 (rator-aval (flowmap-lookup flowmap (anf-var->name (anf-app->rator anf-app))))
		 (rator-vec (aval->pps rator-aval))
		 (rands-avals (map (lambda (var) (flowmap-lookup flowmap var))
				   (map anf-var->name (anf-app->rands anf-app))))
		 (rpass (anf-app->rpass anf-app))
		 (rreturn (anf-app->rreturn anf-app)))
	    (anf-lambda-loop!
	     (lambda (j obj)
	       (if (present? (vector-ref rator-vec j))
		   (let* ((new-pass
			   (RRR flowmap
				(list (anf-get-astore anf-app))
				(aval-union-elements rands-avals (list j))))
			  (new-return
			   (RRR flowmap
				(map anf-get-astore (anf-lambda->last obj))
				(aval-union-elements
				 (cons new-pass
				       (map (lambda (anf-unit)
					      (flowmap-lookup flowmap
							      (anf-var->name (anf-unit->body anf-unit))))
					    (anf-lambda->last obj)))
				 '()))))
		     (vector-set! rpass j new-pass)
		     (vector-set! rreturn j new-return))
		   (begin
		     (vector-set! rpass j aval-bottom)
		     (vector-set! rreturn j aval-bottom)))))
	    (loop (+ i 1)))
	  'finished))))

(define (aval-union-elements aval* index*)
  (let* ((result (new-aval))
	 (result-vec (aval->pps result)))
    (let loop ((aval* aval*))
      (if (null? aval*)
	  'finished
	  (let ((vec (aval->pps (car aval*))))
	    (let rec ((i 0))
	      (if (< i *anf-pp-count*)
		  (begin
		    (if (present? (vector-ref vec i))
			(vector-set! result-vec i present))
		    (rec (+ i 1)))))
	    (loop (cdr aval*)))))
    (let loop ((index* index*))
      (if (null? index*)
	  'finished
	  (begin
	    (vector-set! result-vec (car index*) present)
	    (loop (cdr index*)))))
    result))

(define (RRR flowmap astore* aval)
  (let* ((current (new-aval))
	 (current-vec (aval->pps current)))
    (anf-lambda-loop!
     (lambda (i obj)
       (vector-set! current-vec i
		    (list (lambda ()
			    (for-each (lambda (var)
					;;(display "!!! point 9: var = ") (display var) (newline)
					(add-subset! (flowmap-lookup flowmap var)
						     current))
				      (anf-lambda->free obj)))))))
    (anf-ref-loop!
     (lambda (i obj)
       (vector-set! current-vec i
		    (list (lambda ()
			    (for-each (lambda (astore)
					(add-subset! (astore-ref astore i)
						     current))
				      astore*))))))
    (anf-vector-loop!
     (lambda (i obj)
       (vector-set! current-vec i
		    (list (lambda ()
			    (for-each (lambda (astore)
					(add-subset! (astore-ref astore i)
						     current))
				      astore*))))))
    (anf-ctor-loop!
     (lambda (i obj)
       (vector-set! current-vec i
		    (list (lambda ()
			    (for-each (lambda (anfvar)
					(add-subset! (flowmap-lookup flowmap (anf-var->name anfvar))
						     current))
				      (anf-ctor->actuals obj)))))))
    ;; now toss the first domino
    (add-subset! aval current)
    ;; cleanup
    (let loop ((i 0))
      (if (< i *anf-pp-count*)
	  (begin
	    (if (not (present? (vector-ref current-vec i)))
		(vector-set! current-vec i not-present))
	    (loop (+ i 1)))))
    current))

;;; store constraints
(define (add-astore-subset! astore1 astore2)
  (add-astore-pred-subset! astore1 (lambda (ref) #t) astore2))

(define (add-astore-constrained-subset! astore1 refs astore2)
  (add-astore-pred-subset! astore1 (lambda (ref) (memq ref refs)) astore2))

(define (add-astore-complement-constrained-subset! astore1 refs astore2)
  (add-astore-pred-subset! astore1 (lambda (ref) (not (memq ref refs))) astore2))

(define (add-astore-pred-subset! astore1 pred astore2)
  (let ((worker (lambda (i aval1)
		  (if (pred i)
		      (add-subset! aval1 (astore-ref astore2 i))))))
  (astore-walk astore1 worker)
  (astore-add-neighbor astore1 worker)))

;;; improved store constraints generation
(define (store-constraints-d* def* flowmap)
  (for-each (lambda (d) (store-constraints-d d flowmap)) def*))

(define (store-constraints-d def flowmap)
  (store-constraints-e (anf-def->body def) flowmap))

(define (store-constraints-e anf flowmap)
  (let* ((flowmap-lookup (lambda (key) (flowmap-lookup flowmap key)))
	 (anfvar-lookup (lambda (anfvar) (flowmap-lookup (anf-var->name anfvar)))))
    (let loop ((anf anf))
      (cond
       ((anf-let? anf)
	(let* ((header (anf-let->header anf))
	       (formal (anf-let->formal anf))
	       (body (anf-let->body anf))
	       (rcm-before (anf-get-refcardmap anf))
	       (rcm-after (anf-get-refcardmap body))
	       (astore-before (anf-get-astore anf))
	       (astore-after (anf-get-astore body))
	       (init (anf-get-init anf)))
	  (cond
	   ((anf-var? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after))))
	   ((anf-const? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after))))
	   ((anf-op? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after))))
	   ((anf-cond? header)
	    (let ((anf-then (anf-cond->then header))
		  (anf-else (anf-cond->else header)))
	      (if-init-thunk!
	       init
	       (lambda ()
		 (cm-leq! rcm-before (anf-get-refcardmap anf-then))
		 (cm-leq! rcm-before (anf-get-refcardmap anf-else))
		 (add-astore-subset! astore-before (anf-get-astore anf-then))
		 (add-astore-subset! astore-before (anf-get-astore anf-else))
		 (for-each (lambda (last-anf)
			     (if-init-thunk!
			      (anf-get-init last-anf)
			      (lambda ()
				(cm-leq! (anf-get-refcardmap last-anf) rcm-after)
				(add-astore-subset! (anf-get-astore last-anf) astore-after))))
			   (append (anf-cond->last-then header)
				   (anf-cond->last-else header)))))
	      (loop anf-then)
	      (loop anf-else)))
	   ((anf-call? header)		;have been eliminated in favor of APP
	    'handled-differently)	;this has some expense, but is much simpler
	   ((anf-ctor? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after))))
	   ((anf-sel? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after))))
	   ((anf-lambda? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after)))
	    (loop (anf-lambda->body header)))
	   ((anf-app? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (let ((rator-aval (anfvar-lookup (anf-app->rator header)))
		     (rpass (anf-app->rpass header))
		     (rreturn (anf-app->rreturn header)))
		 (anf-lambda-loop!
		  (lambda (i obj)
		    (let ((thunk
			   (lambda ()
			     (let* ((lp (aval-refsof (vector-ref rpass i)))
				    (lr (aval-refsof (vector-ref rreturn i)))
				    (formals (anf-lambda->formals obj))
				    (body (anf-lambda->body obj))
				    (astore-before-body (anf-get-astore body))
				    (rcm-before-body (anf-get-refcardmap body))
				    (last* (anf-lambda->last obj))
				    (rcm-after-body* (map anf-get-refcardmap last*))
				    (astore-after-body* (map anf-get-astore last*)))
			       (add-astore-constrained-subset! astore-before lp
							       astore-before-body)
			       (cm-restricted-leq! rcm-before lp
						   rcm-before-body)
			       (for-each
				(lambda (last rcm-after-body astore-after-body)
				  (if-init-thunk!
				   (anf-get-init last)
				   (lambda ()
				     (add-astore-complement-constrained-subset!
				      astore-before lp
				      astore-after)
				     (add-astore-constrained-subset!
				      astore-after-body lr
				      astore-after)
				     (cm-restricted-double-join! rcm-before lp
								 (list rcm-after-body) lr
								 #f
								 rcm-after))))
				last*
				rcm-after-body*
				astore-after-body*)))))
		      (add-conditional!-internal i rator-aval thunk))))))))
	   ((anf-ref? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-join-single! rcm-before (anf-ref->nr header) rcm-after))))
	   ((anf-deref? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after))))
	   ((anf-assign? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (let*
		   ((ref-aval (anfvar-lookup (anf-assign->ref header)))
		    (refsof (aval-refsof ref-aval))
		    (ref-count (length refsof))
		    (the-ref 66666)
		    (thunk1 (lambda ()
			      (add-subset! (astore-ref astore-before the-ref)
					   (astore-ref astore-after the-ref))))
		    (worker1 (lambda (ref cm-card)
			       (if (and (eq? ref the-ref)
					(eq? cm-card cardinality-multiple))
				   (thunk1))))
		    (thunk0 (lambda ()
			      (set! ref-count (+ ref-count 1))
			      (cond
			       ((= ref-count 1)
				(set! the-ref (car (aval-refsof ref-aval)))
				(add-astore-complement-constrained-subset! astore-before
									   (list the-ref)
									   astore-after)
				(let ((card (cardmap-lookup rcm-before the-ref)))
				  (if (eq? card cardinality-multiple)
				      (thunk1)
				      ;; if the cardinality increases;
				      ;; other references are dealt with
				      (cardmap->neighbors! rcm-before
							   (cons worker1
								 (cardmap->neighbors rcm-before))))))
			       ((= ref-count 2)
				(thunk1))
			       (else
				'nothing-to-do)))))
		 (cond
		  ((> ref-count 1)
		   (add-astore-subset! astore-before astore-after))
		  ((= ref-count 1)
		   (set! the-ref (car refsof))
		   (add-astore-complement-constrained-subset! astore-before
							      (list the-ref)
							      astore-after)
		   (let ((card (cardmap-lookup rcm-before the-ref)))
		     (if (eq? card cardinality-multiple)
			 (thunk1)
			 (let ((ref-vec (aval->pps ref-aval)))
			   ;; if any other reference arrives
			   (anf-ref-loop!
			    (lambda (i obj)
			      (if (not (= the-ref i))
				  (vector-set! ref-vec i
					       (cons thunk1 (vector-ref ref-vec i))))))
			   ;; or the cardinality increases
			   (cardmap->neighbors! rcm-before
						(cons worker1
						      (cardmap->neighbors rcm-before)))))))
		  (else			;(zero ref-count)
		   (let ((ref-vec (aval->pps ref-aval)))
		     (anf-ref-loop!
		      (lambda (i obj)
			(vector-set! ref-vec i (cons thunk0 (vector-ref ref-vec i)))))))))
	       (cm-leq! rcm-before rcm-after))))
	   ;; vectors
	   ((anf-vector? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after)
	       (cm-set! (anf-vector->nr header) rcm-after cardinality-multiple))))
	   ((anf-vector-ref? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after))))
	   ((anf-vector-set? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after))))
	   ((anf-celleq? header)
	    (if-init-thunk!
	     init
	     (lambda ()
	       (add-astore-subset! astore-before astore-after)
	       (cm-leq! rcm-before rcm-after))))
	   (else
	    (error "not supported" anf))))
	(loop (anf-let->body anf)))
       ((anf-unit? anf)
	'nothing-to-do)
       (else
	(error "anf syntax error" anf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display all cardmaps
(define (all-cardmap-display get-cardmap title)
  (for-each (lambda (anf)
	      (display title)
	      (display " {")
	      (if (anf-let? anf)
		  (display (anf-let->nr anf))
		  (display (anf-unit->nr anf)))
	      (display "} = ")
	      (cardmap-display (get-cardmap anf))
	      (newline))
	    *anf-let/units*)
  (newline))

(define (refcardmap-display)
  (all-cardmap-display anf-get-refcardmap "CellCardMap"))

(define (varcardmap-display)
  (all-cardmap-display anf-get-varcardmap "VarCardMap"))

(define (latex-display-all-cardmap get-cardmap title)
  (display "\\begin{flushleft}") (newline)
  (display "\\begin{tabular}{lcl}") (newline)
  (for-each (lambda (anf)
	      (display title)
	      (display " \\{")
	      (if (anf-let? anf)
		  (display (anf-let->nr anf))
		  (display (anf-unit->nr anf)))
	      (display "\\} &=& ")
	      (latex-display-cardmap (get-cardmap anf))
	      (display "\\\\"))
	    *anf-let/units*)
  (display "\\end{tabular}")
  (display "\\end{flushleft}")
  (newline))

(define (latex-display-refcardmap)
  (latex-display-all-cardmap anf-get-refcardmap "CellCardMap"))

(define (latex-display-varcardmap)
  (latex-display-all-cardmap anf-get-varcardmap "VarCardMap"))

(define (all-init-display)
  (display "InitFS")
  (for-each (lambda (anf)
	      (if (present? (vector-ref (anf-get-init anf) 0))
		  (begin
		    (display " ")
		    (if (anf-let? anf)
			(display (anf-let->nr anf))
			(display (anf-unit->nr anf))))))
	    *anf-let/units*)
  (newline) (newline))

(define latex-display-all-init all-init-display)

(define (all-astore-display)
  (for-each (lambda (anf)
	      (display "AStore {")
	      (if (anf-let? anf)
		  (display (anf-let->nr anf))
		  (display (anf-unit->nr anf)))
	      (display "} = ")
	      (astore-display (anf-get-astore anf))
	      (newline))
	    *anf-let/units*)
  (newline))

(define (astore-display astore)
  (let ((vec (astore->vec astore)))
    (let loop ((i 0))
      (if (< i *anf-pp-count*)
	  (let ((pp (vector-ref *anf-pp-map* i)))
	    (if (or (anf-ref? pp) (anf-vector? pp))
		(let ((entry (vector-ref vec i)))
		  (if entry
		      (begin
			(if (anf-ref? pp)
			    (display " r")
			    (display " v"))
			(display i)
			(display " -> ")
			(aval-display entry)
			(display ",")))))
	    (loop (+ i 1)))))))

(define (latex-display-all-astore)
  (display "\\begin{flushleft}") (newline)
  (display "\\begin{tabular}{lcl}") (newline)
  (for-each (lambda (anf)
	      (display "AStore \\{")
	      (if (anf-let? anf)
		  (display (anf-let->nr anf))
		  (display (anf-unit->nr anf)))
	      (display "\\} &=&")
	      (latex-display-astore (anf-get-astore anf))
	      (display "\\\\"))
	    *anf-let/units*)
  (newline)
  (display "\\end{tabular}")
  (display "\\end{flushleft}")
  (newline))

(define (latex-display-astore astore)
  (display "$\\{")
  (let ((vec (astore->vec astore)))
    (let loop ((i 0))
      (if (< i *anf-pp-count*)
	  (let ((pp (vector-ref *anf-pp-map* i)))
	    (if (or (anf-ref? pp) (anf-vector? pp))
		(let ((entry (vector-ref vec i)))
		  (if entry
		      (begin
			(if (anf-ref? pp)
			    (display "r")
			    (display "v"))
			(display i)
			(display " \\mapsto ")
			(latex-display-aval entry)
			(display ", ")))))
	    (loop (+ i 1))))))
  (display "\\}$"))

;;; analysis of cardinality maps
(define (analyze-cardmap fm-maker get-cardmap)
  (let* ((single (fm-maker))
	 (multiple (fm-maker))
	 (fm-ref (lambda (fm key)
		   (let ((res (fm-lookup fm key)))
		     (if res (cdr res)
			 (begin
			   (fm-update! fm key 0)
			   0)))))
	 (fm-inc (lambda (fm key)
		   (fm-update! fm key (+ 1 (fm-ref fm key))))))
    (for-each (lambda (anf)
		(cardmap-walk
		 (get-cardmap anf)
		 (lambda (key card)
		   (if (cardinality? card)
		       (if (eq? card cardinality-single)
			   (begin (fm-inc single key) (fm-ref multiple key))
			   (begin (fm-inc multiple key) (fm-ref single key)))))))
	      *anf-let/units*)
    (list single multiple)))

(define (analyze-all-varcardmap)
  (analyze-cardmap
   (lambda () (make-symbol-fm '()))
   anf-get-varcardmap))

(define (analyze-all-refcardmap)
  (analyze-cardmap
   (lambda () (make-number-fm '()))
   anf-get-refcardmap))

(define (display-cardmap-analysis keys-to-mark single-multiple-map)
  (let ((single (car single-multiple-map))
	(multiple (cadr single-multiple-map))
	(fm-ref (lambda (fm key)
		   (let ((res (fm-lookup fm key)))
		     (if res (cdr res) 0)))))
    (display "CARDMAP ANALYSIS") (newline)
    (display "    #single #multiple") (newline)
    (fm-walk
     single
     (lambda (key value)
       (display key)
       (display "[")
       (display (countq key keys-to-mark))
       (display "]= ")
       (display value)
       (display "     ")
       (display (fm-ref multiple key))
       (newline)))
    (newline)))

(define (display-analyze-varcardmap vars-to-mark)
  (display-cardmap-analysis vars-to-mark (analyze-all-varcardmap)))

(define (display-analyze-refcardmap)
  (display-cardmap-analysis '() (analyze-all-refcardmap)))

;;; gather free variable statistic
(define (display-free-variable-statistics)
  (anf-lambda-loop!
   (lambda (i obj)
     (let* ((freevars (anf-lambda->free obj))
	    (body (anf-lambda->body obj))
	    (varcardmap (anf-get-varcardmap body)))
       (for-each (lambda (var)
		   (let ((card (cardmap-lookup varcardmap var)))
		     (if (cardinality? card)
			 (display-line card))))
		 freevars)))))

