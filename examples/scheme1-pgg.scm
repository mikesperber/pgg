;;; an interpreter for Scheme1
;;;
;;; $Header$
;;; $Log$
;;; Revision 1.2  1996/06/03 07:35:07  thiemann
;;; checkpoint after including new equational BTA
;;;
;;;Revision 1.7  1995/05/17  12:45:24  thiemann
;;;Added data activation, cleaned up handling of primitive operators
;;;
;;;Revision 1.6  1995/05/15  14:35:58  thiemann
;;;Added partially static data structures
;;;
;;;Revision 1.5  1995/05/04  10:15:23  thiemann
;;;cleaned up treatment of conditional to yield more natural residual
;;;programs.
;;;
;;;Revision 1.4  95/03/30  23:51:44  guest06
;;;version which works for cps programs
;;;
;;;Revision 1.3  95/03/29  13:42:22  guest06
;;;changed closure representation to pairs of abstraction number and
;;;list of values of the free variables
;;;
;;;Revision 1.2  95/03/27  12:56:18  guest06
;;;added flow analysis
;;;
;;; Revision 1.1  1995/01/27  01:56:41  thiemann
;;; Initial revision
;;;

;;; concrete syntax

;;; E ::= V | K | (if E1 E2 E3)  | (O E^*) | (P E^*) | (E E^*) |
;;;       (lambda (V^*) E) 

;;; preprocessed and simplified abstract syntax

;;; E ::= V | K | (if E1 E2 E3)  | (call P E*) | (O E*) |
;;;       (lambda V E) | (apply E1 E2) 

(define-operator (dynamic-car d) d)
(define-operator (dynamic-cdr d) d)

;;;(loadt "scheme1.adt")
;;;(loadt "list.adt")
(defdata mylist
  (my-cons my-car my-cdr)
  (my-nil))
;;;(loadt "closure.adt")
(defdata myclosure
  (mk-error)
  (mk-value   get-value)
  (mk-closure closure-nr closure-vars)
  (mk-data    data-nr    data-args)
  (mk-constant constant-places constant-arg))
;;;(loadt "flow.adt")
;;;(loadt "cltable.adt")
;;;(loadt "reach.adt")
;;;(loadt "oca.adt")
;;;(loadt "intset.adt")

;;;(loadt "debug.adt")

;;; tagging, s1-list->my-list-by, type propagation through contexts
;;; needs (standard-memoization-off)
(defmemo _sim-memoize only)

(define (s1-2int P XS* XD*)
  (let* ((main (car P))
	 (name (s1-get-proc-name main))
	 (args (s1-get-proc-args main))
	 (newargs (let loop ((XS* XS*) (args args))
		    (if (null? XS*) args
			(loop (cdr XS*) (cdr args)))))
	 (activeargs (let loop ((XS* XS*))
		       (if (null? XS*) '()
			   (cons (s1-activate (car XS*)) (loop (cdr XS*))))))
	 (call (append (list 'call name)
		       activeargs newargs))
	 (newmain (list 'define (cons 0 newargs) call)))
  (s1-int (cons newmain P) XD*)))

;;; transform data to an expression yielding that data
;;; all quoted expressions should probably be activated?!
;;; yes, that simplifies matters considerably!
(define (s1-activate d)
  (list 'quote d))

;;; build quoted first-order data
;;; bt: S -> Ps
(define (s1-build-data d)
  (cond
   ((not (list? d))
    (mk-constant '() d))
   ((null? d)
    (mk-data #f (my-nil)))
   (else
    (mk-data #f (my-cons (s1-build-data (car d))
			 (s1-build-data (cdr d)))))))


;;; here the static input is a list of static skeletons of the
;;; specialized program's input. Dynamic parts are marked by '***
;;; (what else :-)
;;; Hmm, might be better to call them cv1 to cvn?
(define (s1-2int-skeleton P call-XS* XD*)
  (let* ((procname (car call-XS*))
	 (skeletons (cdr call-XS*))
	 (main (s1-lookup-defn P procname))
	 (formals (s1-get-proc-args main))
	 (expr/count (s1-skeleton->expression* skeletons 0))
	 (newformals (let loop ((n (- (my-cdr expr/count) 1))
				(acc '()))
		       (if (< n 0)
			   acc
			   (loop (- n 1) (cons (s1-skeleton-var n) acc)))))
	 (call (append (list 'call procname) (my-car expr/count)))
	 (newmain (list 'define (cons 0 newformals) call)))
  (s1-int (cons newmain P) XD*)))

;;; returns (my-cons expression new-count)
(define (s1-skeleton->expression skeleton count)
  (cond
   ((pair? skeleton)
    (let* ((ec-car (s1-skeleton->expression (car skeleton) count))
	   (ec-cdr (s1-skeleton->expression (cdr skeleton) (my-cdr ec-car))))
      (my-cons (list 'CONS (my-car ec-car) (my-car ec-cdr))
	       (my-cdr ec-cdr))))
   ((equal? '*** skeleton)
    (my-cons (s1-skeleton-var count) (+ count 1)))
   ((symbol? skeleton)
    (my-cons (list 'QUOTE skeleton) count))
   (else
    (my-cons skeleton count))))

(define (s1-skeleton->expression* skeleton* count)
  (if (null? skeleton*)
      (my-cons '() count)
      (let* ((ec (s1-skeleton->expression (car skeleton*) count))
	     (ec* (s1-skeleton->expression* (cdr skeleton*) (my-cdr ec))))
	(my-cons (cons (my-car ec) (my-car ec*))
		 (my-cdr ec*)))))

(define (s1-skeleton-var n)
  (string->symbol (string-append "v" (number->string n))))

;;; one-level interpreter starts here:

(define (s1-int P X*)
  (let* ((PR (s1-flow-analysis P))
	 (D* (car PR))			;list of annotated definitions
	 (I (s1-cltable D*))		;closure table
	 (R (cdr PR))			;closure -> flows map
	 (T (s1-dyn-analysis D* I R))	;list of dynamic lambdas
	 (O (scheme1-oca D*))			;alist of variable flows/#occurrences
	 (D (car D*))
	 (V* (s1-get-proc-args D))
	 (A* (s1-list->my-list-by V* X*))
	 ;; CALL-BY-NAME:
	 ;;(A* (s1-list->susp-list V* X*))
	 (result (s1-eval D* R I O V* A* T (s1-get-proc-body D))))
    (get-value (s1-dynamize-data result))))

;;; P  - program, list of definitions
;;; V* - list of variable names
;;; A* - my-list of tagged values
;;; T  - list of labels of dynamic lambdas
;;; E  - expression to evaluate
(define (s1-eval P R I O V* A* T E0)
  (let* ((flow-var (s1-flow-of E0))
	 (E (s1-meat-of E0))
	 (dynamic-closures (if T #f #t))) ;just to make T static...

;;; NESTED stuff:

;;; closures that are dynamized can't really be nested
;;; NEW: dynamize everything:

(define (dynamize-everything cl)
  (mk-value
   (let loop ((v cl))
     (if (mk-closure? v)
	 (if dynamic-closures
	     (cons (closure-nr v)
		   (let var-loop ((v* (closure-vars v)))
		     (if (my-nil? v*)
			 '()
			 (cons (loop (my-car v*))
			       (var-loop (my-cdr v*))))))
	     (let* ((clexp (s1-cltable:nr->exp I (closure-nr v)))
		    (clarg (s1-meat-of (s1-get-lambda-arg clexp)))
		    (clbody (s1-get-lambda-body clexp))
		    (V0* (s1-set-subtract (s1-freevars clbody) clarg))
		    (A0* (closure-vars v)))
	     (lambda (A)
	       (loop
		(s1-eval P R I O (cons clarg V0*) (my-cons (mk-value A) A0*) T clbody)))))
	 (if (mk-data? v)
	     (let ((args (data-args v)))
	       (if (my-nil? args)
		   '()
		   (cons (loop (my-car args))
			 (loop (my-cdr args)))))
	     (if (mk-constant? v)
		 (constant-arg v)
		 (get-value v)))))))

;;; check if data has nested creation points and dynamize at those points 
(define (dynamize-when-needed d)
  ;;(dynamize-top-down d)
  (dynamize-bottom-up d)
  )

;;; two implementations: 
;;; top-down generalizes AT THE END of every path with a repetition
;;; bottom-up generalizes AT THE BEGINNING of every tree with a repetition
(define (dynamize-top-down d)
  (let loop ((d d) (dcps '()))		;keep track of data creation points
    (cond
     ((mk-value? d)
      d)
     ((mk-closure? d)
      (let ((dcp (closure-nr d)))
	(if (member dcp dcps)
	    (dynamize-everything d)
	    (mk-closure dcp
			(let var-loop ((vars (closure-vars d)))
			  (if (my-nil? vars)
			      (my-nil)
			      (my-cons (loop (my-car vars) (cons dcp dcps))
				       (var-loop (my-cdr vars)))))))))
     ((mk-data? d)
      (let ((dcp (data-nr d)))
	(if (and dcp (member dcp dcps))
	    (dynamize-everything d)
	    (mk-data dcp
		     (let ((args (data-args d)))
		       (if (my-nil? args)
			   (my-nil)
			   (my-cons (loop (my-car args) (cons dcp dcps))
				    (loop (my-cdr args) (cons dcp dcps)))))))))
     ((mk-constant? d)
      d)
     (else
      (mk-error)))))

(define (dynamize-bottom-up d)
  (my-car
   ;; the loop returns the generalized datum paired with the set of
   ;; data creation points of the original datum
   (let loop ((d d))		;keep track of data creation points
     (cond
      ((mk-value? d)
       (my-cons d '()))
      ((mk-closure? d)
       (let var-loop
	   ((vars (closure-vars d))
	    (dynamized-vars (my-nil))
	    (data-creation-points '()))
	 (if (my-nil? vars)
	     (let* ((dcp (closure-nr d))
		    (new-d (mk-closure dcp (s1-my-reverse dynamized-vars)))
		    (new-dcps (s1-insert dcp data-creation-points)))
	      (if (member dcp data-creation-points)
		  (my-cons (dynamize-everything new-d) '())
		  ;; one might argue that "new-dcps" in place of '()
		  ;; above is a more conservative choice
		  (my-cons new-d new-dcps)))
	     (let ((result (loop (my-car vars))))
	       (var-loop (my-cdr vars)
			 (my-cons (my-car result) dynamized-vars)
			 (s1-union (my-cdr result) data-creation-points))))))
     ((mk-data? d)
      (let ((dcp (data-nr d)))
	(if dcp
	    (let ((args (data-args d)))
	      (if (my-nil? args)
		  (my-cons d (list (data-nr d)))
		  (let* ((result-car (loop (my-car args)))
			 (result-cdr (loop (my-cdr args)))
			 (new-d (mk-data dcp (my-cons (my-car result-car)
						      (my-car result-cdr))))
			 (data-creation-points (s1-union (my-cdr result-car)
						      (my-cdr result-cdr))))
		    (if (member dcp data-creation-points)
			(my-cons (dynamize-everything new-d) '())
			(my-cons new-d (s1-insert dcp
					       data-creation-points))))))
	    (my-cons d '()))))
     ((mk-constant? d)
      (my-cons d '()))
     (else
      (my-cons (mk-error) '()))))))

;;; environment restriction & dynamization
;;; (V*, A*) - the original environment
;;; V0*      - the set of variables to project on

(define (restrict-and-dynamize V* A* V0*)
  (if (null? V0*)
      (my-nil)
      (let ((V (car V0*)))
	(my-cons (dynamize-when-needed (s1-lookup V* A* V))
		 (restrict-and-dynamize V* A* (cdr V0*))))))

(define (restrict V* A* V0*)
  (if (null? V0*)
      (my-nil)
      (let ((V (car V0*)))
	(my-cons (s1-lookup V* A* V) (restrict V* A* (cdr V0*))))))

;;; create a list of suspensions
(define (s1-suspend E0)
  (let* ((V0* (s1-freevars E0))
	 (A0* (restrict-and-dynamize V* A* V0*))
	 (susp (dynamize-when-needed (mk-closure (s1-flow-of E0) A0*))))
    susp))

(define (s1-suspend* E*)
  (if (null? E*)
      (my-nil)
      (let* ((susp (s1-suspend (car E*))))
	(my-cons susp (s1-suspend* (cdr E*))))))

    ;; MAIN EVALUATION LOOP STARTS HERE

    ;;(nl) (dpy "[s1-eval ") (dpy E) (dpy " ") (dpy V*) (nl)
    (cond
     ((s1-is-V? E)
      (s1-lookup V* A* E))
     ((s1-is-V? E)				; CALL-BY-NAME
      (let* ((susp (s1-lookup V* A* E))
	     (clnr (closure-nr susp))
	     (clexp (if (number? clnr)
			(s1-cltable:nr->exp I (closure-nr susp))
			clnr))
	     (clvars (closure-vars susp)))
	(s1-eval P R I O (s1-freevars clexp) clvars T clexp)))
     ((not (list? E))
      (mk-constant '() E))
     ((null? E)
      (mk-data flow-var (my-nil)))
     ((equal? 'QUOTE (car E))
      (s1-build-data (s1-get-quote-body E)))
     ((equal? 'IF (car E))
    (let ((b (s1-eval P R I O V* A* T (s1-get-if-cond E)))
	  (V0* (s1-freevars* (cddr E))))
      (cond
       ((mk-constant? b)
	;;(nl) (dpy "constant-arg b = ") (dpy (constant-arg b))
	(if (constant-arg b)
	    (s1-eval P R I O V* A* T (s1-get-if-then E))
	    (s1-eval P R I O V* A* T (s1-get-if-else E))))
       ((not (mk-value? b))
	(s1-eval P R I O V* A* T (s1-get-if-then E)))
       (else
	(let ((V* V0*)
	      (A* (restrict-and-dynamize V* A* V0*)))
	  (nl) (dpy "dynamic conditional: ") (dpy E) (nl)
	  (mk-value
	   (_sim-memoize 1
	    (if (get-value b)
		(get-value (dynamize-everything (s1-eval P R I O V* A* T (s1-get-if-then E))))
		(get-value (dynamize-everything (s1-eval P R I O V* A* T (s1-get-if-else E))))))))))))
   ((equal? 'CALL (car E))
    (let ((D (s1-lookup-defn P (s1-get-call-proc E))))
      (s1-eval P R I O
	       (s1-get-proc-args D)
	       (s1-eval* P R I O V* A* T (s1-get-call-args E))
	       T
	       (s1-get-proc-body D)
	       )))
   ((equal? 'CALL (car E))		; CALL-BY-NAME
    (let ((D (s1-lookup-defn P (s1-get-call-proc E))))
      (s1-eval P R I O
	       (s1-get-proc-args D)
	       (s1-suspend* (s1-get-call-args E))
	       T
	       (s1-get-proc-body D)
	       )))
   ((equal? 'LAMBDA (car E))
    (let* ((V0* (s1-freevars E0))
	   (A0* (restrict V* A* V0*))	;restrict-and-dynamize?
	   (cl  (mk-closure flow-var A0*))) ;(dynamize-when-needed ...)
      (if #f ;(member flow-var T)
	  (if dynamic-closures
	      (dynamize-everything cl)
	      (let ((clarg (s1-meat-of (s1-get-lambda-arg E)))
		    (clbody (s1-get-lambda-body E)))
		(mk-value
		 (lambda (A)
		   (get-value
		    (dynamize-everything
		     (s1-eval P R I O (cons clarg V0*) (my-cons (mk-value A) A0*) T clbody)))))))
	  cl)))
   ((equal? 'APPLY (car E))
    (let* ((rator (s1-eval P R I O V* A* T (s1-get-apply-rator E)))
	   (V0*   (s1-freevars (s1-get-apply-rand E)))
	   (A0*   (restrict V* A* V0*))	;restrict-and-dynamize?
	   (rand  (s1-eval P R I O V0* A0* T (s1-get-apply-rand E)))
	   (ratorinfo (assoc (s1-flow-of (s1-get-apply-rator E)) O))
	   ;; CALL-BY-NAME:
	   ;; (rand  (s1-suspend (get-apply-rand E)))
	   )
      (let ((d-lambdas (s1-all-functions (s1-flow-of (s1-get-apply-rator E)) R I)))
	;;(nl) (dpy "apply: ") (dpy (mk-closure? rator)) (dpy d-lambdas) (nl)
	(if (mk-closure? rator) ;(null? d-lambdas)
	    ;; apply a static lambda
	    (let* ((cl     rator)
		   (clexp  (s1-cltable:nr->exp I (closure-nr cl)))
		   (clarg  (s1-meat-of (s1-get-lambda-arg clexp)))
		   (clbody (s1-get-lambda-body clexp))
		   (clvars (closure-vars cl))
		   (newformals (cons clarg
				  (s1-set-subtract (s1-freevars clbody) clarg)))
		   (newactuals (my-cons rand clvars))
		   (res 
		    (if (and ratorinfo
			     (> 1 (cdr ratorinfo)))
			(mk-value
			 (_sim-memoize 1
			  (get-value
			   (dynamize-everything
			    (s1-eval P R I O
				     newformals
				     newactuals
				     T
				     clbody)))))
			(s1-eval P R I O
				 newformals
				 newactuals
				 T
				 clbody))))
	      res)
	    ;; apply a dynamic lambda
	    (if dynamic-closures
		;; represented as a dynamic closure
		(let* ((dyn-cl (get-value rator))	;unprotected
		       (dyn-nr (car dyn-cl)))
		  (let loop ((d-lambdas d-lambdas))
		    (mk-value
		     (_sim-memoize 1
		      (get-value
		       (if (or (null? (cdr d-lambdas))
			       (equal? (car d-lambdas) dyn-nr))
			   (let* ((clexp (s1-cltable:nr->exp I (car d-lambdas)))
				  (clarg (s1-meat-of (s1-get-lambda-arg clexp)))
				  (clbody (s1-get-lambda-body clexp))
				  (fvs (s1-set-subtract (s1-freevars clbody) clarg))
				  (clvars (s1-list->my-list-by fvs (cdr dyn-cl))))
			     ;;(dpy "[dyn-apply: (") (dpy clarg) (dpy ") ") (dpy fvs) (dpy " ") (dpy clbody) (nl)
			     (dynamize-everything
			      (s1-eval P R I O
				       (cons clarg fvs)
				       (my-cons rand clvars)
				       T
				       clbody)))
			   (loop (cdr d-lambdas))
			   )))))
		  )
		;; apply a "real" dynamic function
		(mk-value ((get-value rator) ; unprotected
			   (get-value (dynamize-everything rand))))
		)
	    ))))
   (else
    (s0-apply-op (s1-get-op-name E) flow-var
		 (s1-eval* P R I O V* A* T (s1-get-op-args E))))
   )))

(define (s1-eval* P R I O V* A* T E*)
  ;(display "s1-eval* ") (display E*) (newline)
  (if (null? E*)
      (my-nil)
      (my-cons (s1-eval P R I O V* A* T (car E*))
	       (s1-eval* P R I O V* A* T (cdr E*)))))

;;; variable lookup
(define (s1-lookup V* A* V)
  ;;(dpy "[lookup (") (dpy v) (dpy ") ") (dpy V*) (dpy (s1-my-length A*)) (nl)
  (let ((res
	 (let loop ((V* V*) (A* A*))
	   (if (null? V*)
      (begin ;;(dpy "->error") (nl)
	(mk-error))
      (if (equal? (car V*) V)
	  (my-car A*)			;unprotected
	  (loop (cdr V*) (my-cdr A*)))))))
    ;;(dpy "returns]") (nl)
    res))

(define (s1-lookup* V* A* V0*)
  (if (null? V0*)
      (my-nil)
      (my-cons (s1-lookup V* A* (car V0*))
	       (s1-lookup* V* A* (cdr V0*)))))

;;; apply  operator oo to argument list
(define (s0-apply-op oo flow-var args)
  (dpy "s0-apply-op ") (dpy oo)
  (cond
   ((my-nil? args)
    (dpy " <no args>"))
   ((mk-constant? (my-car args))
    (dpy " constant ") (dpy (constant-arg (my-car args))))
   ((mk-data? (my-car args))
    (dpy " data"))
   ((mk-value? (my-car args))
    (dpy " value"))
   (else
    (dpy " otherwise")))
  (nl)
  (cond
   ;; data constructors & selectors, only applicable to partially static arguments!
   ((equal? 'cons oo)
    (mk-data flow-var (my-cons (my-car args) (my-car (my-cdr args)))))
   ((equal? 'nil oo)
    (mk-data flow-var (my-nil)))
   ((equal? 'car oo)
    (let ((val (my-car args)))
      (if (mk-data? val)
	  (let ((data (data-args val)))
	    (if (my-nil? data)
		(mk-value (dynamic-car '()))
		(my-car (data-args val))))
	  (mk-value (car (get-value val))))))
   ((equal? 'cdr oo)
    (let ((val (my-car args)))
      (if (mk-data? val)
	  (let ((data (data-args val)))
	    (if (my-nil? data)
		(mk-value (dynamic-cdr '()))
		(my-cdr (data-args val))))
	  (mk-value (cdr (get-value val))))))
   ((equal? 'null? oo)
    (let ((val (my-car args)))
      (cond
       ((mk-data? val)
	(mk-constant '() (my-nil? (data-args val))))
       ((mk-value? val)
	(mk-value (null? (get-value val))))
       (else
	(mk-constant '() #f)))))
   ((equal? 'pair? oo)
    (let ((val (my-car args)))
      (cond
       ((mk-data? val)
	(mk-constant '() (my-cons? (data-args val))))
       ((mk-value? val)
	(mk-value (pair? (get-value val))))
       (else
	(mk-constant '() #f)))))
   ;; other primitives:
   ((equal? oo (quote equal?))
    (let ((arg1 (my-car args))
	  (arg2 (my-car (my-cdr args))))
      (if (and (mk-constant? arg1) (mk-constant? arg2))
	  (mk-constant '() (equal? (constant-arg arg1) (constant-arg arg2)))
	  (mk-value (equal? (get-value (s1-dynamize-data arg1))
			    (get-value (s1-dynamize-data arg2)))))))
   ((equal? oo (quote =))
    (let ((arg1 (my-car args))
	  (arg2 (my-car (my-cdr args))))
      (if (and (mk-constant? arg1) (mk-constant? arg2))
	  (mk-constant '() (= (constant-arg arg1) (constant-arg arg2)))
	  (mk-value (= (get-value (s1-dynamize-data arg1))
		       (get-value (s1-dynamize-data arg2)))))))
   ((equal? oo (quote <))
    (let ((arg1 (my-car args))
	  (arg2 (my-car (my-cdr args))))
      (if (and (mk-constant? arg1) (mk-constant? arg2))
	  (mk-constant '() (< (constant-arg arg1) (constant-arg arg2)))
	  (mk-value (< (get-value (s1-dynamize-data arg1))
		       (get-value (s1-dynamize-data arg2)))))))
   ((equal? oo (quote >))
    (let ((arg1 (my-car args))
	  (arg2 (my-car (my-cdr args))))
      (if (and (mk-constant? arg1) (mk-constant? arg2))
	  (mk-constant '() (> (constant-arg arg1) (constant-arg arg2)))
	  (mk-value (> (get-value (s1-dynamize-data arg1))
		       (get-value (s1-dynamize-data arg2)))))))
   ((equal? oo (quote *))
    (let ((arg1 (my-car args))
	  (arg2 (my-car (my-cdr args))))
      (if (and (mk-constant? arg1)
	       (mk-constant? arg2)
	       (not (member flow-var (constant-places arg1)))
	       (not (member flow-var (constant-places arg2))))
	  (mk-constant (s1-insert flow-var
			       (s1-union (constant-places arg1)
				      (constant-places arg2)))
		       (* (constant-arg arg1) (constant-arg arg2)))
	  (mk-value (* (get-value (s1-dynamize-data arg1))
		       (get-value (s1-dynamize-data arg2)))))))
   ((equal? oo (quote +))
    (let ((arg1 (my-car args))
	  (arg2 (my-car (my-cdr args))))
      (if (and (mk-constant? arg1)
	       (mk-constant? arg2)
	       (not (member flow-var (constant-places arg1)))
	       (not (member flow-var (constant-places arg2))))
	  (mk-constant (s1-insert flow-var
			       (s1-union (constant-places arg1)
				      (constant-places arg2)))
		       (+ (constant-arg arg1) (constant-arg arg2)))
	  (mk-value (+ (get-value (s1-dynamize-data arg1))
		       (get-value (s1-dynamize-data arg2)))))))
   ((equal? oo (quote -))
    (let ((arg1 (my-car args))
	  (arg2 (my-car (my-cdr args))))
      (if (and (mk-constant? arg1)
	       (mk-constant? arg2)
	       ;(not (member flow-var (constant-places arg1)))
	       ;(not (member flow-var (constant-places arg2)))
	       )
	  (mk-constant (s1-insert flow-var
			       (s1-union (constant-places arg1)
				      (constant-places arg2)))
		       (- (constant-arg arg1) (constant-arg arg2)))
	  (mk-value (- (get-value (s1-dynamize-data arg1))
		       (get-value (s1-dynamize-data arg2)))))))
   ((equal? oo (quote number?))
    (let ((arg (my-car args)))
      (if (mk-constant? arg)
	  (mk-constant '() (number? (constant-arg arg)))
	  (if (mk-value? arg)
	      (mk-value (number? (get-value arg)))
	      (mk-constant '() #f)))))
   ((equal? oo (quote char?))
    (let ((arg (my-car args)))
      (if (mk-constant? arg)
	  (mk-constant '() (char? (constant-arg arg)))
	  (if (mk-value? arg)
	      (mk-value (char? (get-value arg)))
	      (mk-constant '() #f)))))
   ((equal? oo (quote boolean?))
    (let ((arg (my-car args)))
      (if (mk-constant? arg)
	  (mk-constant '() (boolean? (constant-arg arg)))
	  (if (mk-value? arg)
	      (mk-value (boolean? (get-value arg)))
	      (mk-constant '() #f)))))
   ((equal? oo (quote symbol?))
    (let ((arg (my-car args)))
      (if (mk-constant? arg)
	  (mk-constant '() (symbol? (constant-arg arg)))
	  (if (mk-value? arg)
	      (mk-value (symbol? (get-value arg)))
	      (mk-constant '() #f)))))
   ((equal? oo (quote list?))
    (let ((arg (my-car args)))
      (if (mk-data? arg)
	  (mk-constant '() #t)
	  (if (mk-value? arg)
	      (mk-value (list? (get-value arg)))
	      (mk-constant '() #f)))))
   (else
    (mk-value 'error))))

;;; utilities

(define (s1-list->my-list-by ss dd)
  (cond
   ((null? ss)
    (my-nil))
   ((= 1 (length ss))
    (my-cons (mk-value (car dd))
	     (my-nil)))
   (else
    (my-cons (mk-value (car dd))
	     (s1-list->my-list-by (cdr ss) (cdr dd))))))

(define (s1-list->susp-list V* D*)
  (cond
   ((null? V*)
    (my-nil))
   ((= 1 (length V*))
    (my-cons (mk-closure (cons 0 (car V*))
			 (my-cons (mk-value (car D*)) (my-nil)))
	     (my-nil)))
   (else
    (my-cons (mk-closure (cons 0 (car V*))
			 (my-cons (mk-value (car D*)) (my-nil)))
	     (s1-list->my-list-by (cdr V*) (cdr D*))))))

(define (s1-my-length l)
  (if (my-nil? l)
      0
      (+ 1 (s1-my-length (my-cdr l)))))

(define (s1-my-reverse l)
  (define (my-it-rev l a)
    (if (my-nil? l)
	a
	(my-it-rev (my-cdr l) (my-cons (my-car l) a))))
  (my-it-rev l (my-nil)))

;;; dynamize first-order data, 

(define (s1-dynamize-data d)
  (mk-value
   (let loop ((v d))
     (cond
      ((mk-data? v)
       (let ((args (data-args v)))
	 (if (my-nil? args)
	     '()
	     (cons (loop (my-car args))
		   (loop (my-cdr args))))))
      ((mk-constant? v)
       (constant-arg v))
      (else
	 (get-value v))))))
