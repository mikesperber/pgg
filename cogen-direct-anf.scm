;;; cogen-direct-anf.scm

;;; copyright © 1996-2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; 
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;;
;;; includes the conversion of the residual code to A-normal form
;;; hence performs full context propagation
;;;

(set-scheme->abssyn-let-insertion! #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an implementation using macros

(define-syntax _app
  (syntax-rules ()
    ((_app 0 e ...)
     (e ...))
    ((_app 1 e arg ...)
     (_complete-serious e (list arg ...)))
    ((_app lv e ...)
     (_complete (make-residual-generator-ve* '_APP (pred lv) e ...)))))

(define-syntax _app-no-result
  (syntax-rules ()
    ((_app 0 e ...)
     (e ...))
    ((_app 1 e arg ...)
     (_complete-serious-no-result e (list arg ...)))
    ((_app lv e ...)
     (_complete-no-result (make-residual-generator-ve* '_APP (pred lv) e ...)))))

(define-syntax _app_memo
  (syntax-rules ()
    ((_app_memo 0 f arg ...)
     ((f 'VALUE) arg ...))
    ((_app_memo lv e ...)
     (_complete (make-residual-generator-ve* '_APP_MEMO (pred lv) e ...)))))

(define-syntax _lambda
  (syntax-rules ()
    ((_lambda 0 vars vvs bts body)
     (lambda vars body))
    ((_lambda lv vars vvs bts body)
     (_lambda-internal lv 'vars vvs bts (lambda vars body)))))

(define (_lambda-internal lv arity vvs bts f)
  (let* ((vars (map make-residual-variable (map gensym-local arity)))
	 (body (reset (apply f vars)))
	 (l (pred lv))
	 ;; for fvs
	 (lambda-pp (cons 'LAMBDA vvs))
	 (dynamics (top-project-dynamic lambda-pp bts))
	 (compressed-dynamics (remove-duplicates dynamics))
	 (actual-fvs (map car compressed-dynamics))
	 ;; end for fvs
	 (generate-lambda
	  (if (zero? l)
	      (lambda ()
		(make-residual-lambda vars actual-fvs body))
	      (lambda ()
		(let ((new-bts (map pred (map cdr compressed-dynamics))))
		  (make-residual-generator-vveqe '_LAMBDA l vars
						 (make-residual-call 'LIST actual-fvs)
						 new-bts
						 body))))))
    ;; (display-line "_lambda-internal " dynamics)
    (if *lambda-is-pure*
	(generate-lambda)
	(_complete			;don't duplicate, experimental
	 (generate-lambda)))))

(define-syntax _lambda_memo
  (syntax-rules ()
    ((_lambda_memo 0 arity label vvs bts f)
     (static-constructor label f vvs bts))
    ((_lambda_memo arg ...)
     (_lambda_memo-internal arg ...))))

(define (_lambda_memo-internal lv arity label vvs bts f)
  (address-registry-reset!)
  (address-map-reset!)
  (let* ((formals (map make-residual-variable (map gensym-local arity)))
	 (lambda-pp (cons label vvs))
	 (dynamics (top-project-dynamic lambda-pp bts))
	 (compressed-dynamics (remove-duplicates dynamics))
	 (actual-fvs (map car compressed-dynamics))
	 (clone-map (map (lambda (arg)
			   (cons arg (if (symbol? arg)
					 (gensym-local arg)
					 (gensym-local 'clone))))
			 actual-fvs))
	 (cloned-pp (top-clone-with clone-map lambda-pp bts))
	 (cloned-vvs (cdr cloned-pp))
	 (new-bts (map pred (map cdr compressed-dynamics)))
	 (formal-fvs (map cdr clone-map)))
    ;; (> lv 0)
    (_complete
     (make-residual-generator-vqqeqe
      '_LAMBDA_MEMO
      (pred lv)
      arity
      (gensym 'cls)
      (make-residual-call 'LIST actual-fvs)
      new-bts
      (make-residual-closed-lambda
       formal-fvs
       'FREE
       (make-residual-closed-lambda
	formals
	'FREE
	(reset (apply (apply f cloned-vvs) formals))))))))

;;; formerly:
;;;      `(_LAMBDA_MEMO
;;;        ,(- lv 1)
;;;        ',arity
;;;        ',(gensym 'cls)
;;;        (LIST ,@actual-fvs)
;;;        ',new-bts
;;;        (LAMBDA ,formal-fvs
;;; 	 (LAMBDA ,formals
;;; 	   ,(reset (apply (apply f cloned-vvs) formals)))))

(define-syntax _vlambda
  (syntax-rules ()
    ((_vlambda 0 () var body)
     (lambda var body))
    ((_vlambda 0 (fixed-var ...) var body)
     (lambda (fixed-var ... . var) body))
    ((_vlambda lv (fixed-var ...) var body)
     (_vlambda-internal lv '(var fixed-var ...)
			(lambda (var fixed-var ...) body)))))

(define (_vlambda-internal lv arity f)
  (let* ((vars (map make-residual-variable (map gensym-local arity)))
	 (body (reset (apply f vars)))
	 (l (pred lv))
	 (fixed-vars (cdr vars))
	 (var (car vars)))
    (_complete				;don't duplicate, experimental
     (if (zero? l)
	 (make-residual-closed-lambda (append fixed-vars var) '() body)
	 (make-residual-generator-vvve* '_VLAMBDA l fixed-vars var body)))))

(define-syntax _vlambda_memo
  (syntax-rules ()
    ((_vlambda_memo 0 arity var label vvs bts f)
     (static-constructor label f vvs bts))
    ((_vlambda_memo arg ...)
     (_vlambda_memo-internal arg ...))))

(define (_vlambda_memo-internal lv arity var label vvs bts f)
  (address-registry-reset!)
  (address-map-reset!)
  (let* ((fixed-formals (map make-residual-variable (map gensym-local arity)))
	 (formal (gensym-local var))
	 (lambda-pp (cons label vvs))
	 (dynamics (top-project-dynamic lambda-pp bts))
	 (compressed-dynamics (remove-duplicates dynamics))
	 (actual-fvs (map car compressed-dynamics))
	 (clone-map (map (lambda (arg)
			   (cons arg (if (symbol? arg)
					 (gensym-local arg)
					 (gensym-local 'clone))))
			 actual-fvs))
	 (cloned-pp (top-clone-with clone-map lambda-pp bts))
	 (cloned-vvs (cdr cloned-pp))
	 (new-bts (map pred (map cdr compressed-dynamics)))
	 (formal-fvs (map cdr clone-map)))
    ;; (> lv 0)
    (let ((lv (- lv 1)))
      (_complete
       (make-residual-generator-vqqqeqe
	'_VLAMBDA_MEMO
	lv
	arity
	var
	(gensym 'cls)
	(make-residual-call 'LIST actual-fvs)
	new-bts
	(make-residual-closed-lambda
	 formal-fvs
	 'FREE
	 (make-residual-closed-lambda
	  (if (zero? lv)
	      (append fixed-formals formal)
	      (cons formal fixed-formals))
	  'FREE
	  (reset (apply (apply f cloned-vvs) (cons formal fixed-formals))))))))))

;;; was:
;;;        `(_VLAMBDA_MEMO
;;; 	 ,lv
;;; 	 ',arity
;;; 	 ',var
;;; 	 ',(gensym 'cls)
;;; 	 (LIST ,@actual-fvs)
;;; 	 ',new-bts
;;; 	 (LAMBDA ,formal-fvs
;;; 	   (LAMBDA ,(if (zero? lv)
;;; 			(append fixed-formals formal)
;;; 			(cons formal fixed-formals))
;;; 	     ,(reset (apply (apply f cloned-vvs)
;;; 			    (cons formal fixed-formals))))))

(define-syntax _lambda_poly
  (syntax-rules ()
    ((_lambda_poly 0 arity bts body-level label body)
     (poly-constructor
      label
      'arity
      bts body-level
      (lambda arity body)
      (list 'vector)
      (_complete-serious 'vector '()))) ; #### is this right? ---Mike
    ((_lambda_poly level arity bts body-level label body)
     `(_lambda_poly ,(pred level) arity ',(map pred bts) ,(pred body-level) 'label ,body))))

(define-syntax _begin
  (syntax-rules (multi-memo _app _op _op-serious)
    ((_begin 0 bl (multi-memo arg ...) e2)
     (begin (multi-memo-no-result arg ...) e2))
    ((_begin 1 bl (multi-memo arg ...) e2)
     (shift k (make-residual-begin (multi-memo-no-result arg ...)
				   (reset (k e2)))))
    ((_begin 0 bl (_app arg ...) e2)
     (begin (_app-no-result arg ...)
	    e2))
    ((_begin 1 bl (_app arg ...) e2)
     (shift k (make-residual-begin (_app-no-result arg ...) (reset (k e2)))))
    ((_begin 0 bl (_op arg ...) e2)
     (begin (_op-no-result arg ...) e2))
    ((_begin 0 bl (_op-serious arg ...) e2)
     (begin (_op-serious-no-result arg ...) e2))
    ((_begin 1 bl (_op arg ...) e2)
     (shift k (make-residual-begin (_op-no-result arg ...) (reset (k e2)))))
    ((_begin 1 bl (_op-serious arg ...) e2)
     (shift k (make-residual-begin (_op-serious-no-result arg ...) (reset (k e2)))))
    ((_begin 0 bl e1 e2)
     (begin e1 e2))
    ((_begin 1 bl e1 e2)
     (shift k (make-residual-begin e1 (reset (k e2)))))
    ((_begin lv bl e1 e2)
     (shift k (make-residual-generator-vvee '_BEGIN (pred lv) 0 e1 (reset (k e2)))))))

(define-syntax _ctor_memo
  (syntax-rules ()
    ((_ctor_memo 0 bts #f ctor arg ...)
     (static-constructor 'ctor ctor (list arg ...) 'bts))
    ((_ctor_memo 0 bts #t ctor arg ...)
     (hidden-constructor 'ctor ctor (list arg ...) 'bts))
    ((_ctor_memo lv (bt ...) hidden ctor arg ...)
     (_complete
      (make-residual-generator-vvvve* '_CTOR_MEMO
				      (pred lv)
				      (list (pred bt) ...)
				      hidden
				      'ctor
				      arg ...)))))

(define-syntax _s_t_memo
  (syntax-rules ()
    ((_s_t_memo 0 sel v a ...)
     (sel (v 'VALUE) a ...))
    ((_s_t_memo lv sel v a ...)
     (_complete
      (make-residual-generator-vve* '_S_T_MEMO (pred lv) 'sel v a ...)))))

(define-syntax _make-cell_memo
  (syntax-rules ()
    ((_make-cell_memo 0 lab bt arg)
     (static-cell lab arg bt))
    ((_make-cell_memo lv lab bt arg)
     (_complete
      (make-residual-generator-vvve* '_MAKE-CELL_MEMO (pred lv) 'lab (pred bt) arg)))))

(define-syntax _cell-eq?_memo
  (syntax-rules ()
    ((_cell-eq?_memo 0 ref1 ref2)
     (eq? (ref1 'VALUE) (ref2 'VALUE)))
    ((_cell-eq?_memo lv ref1 ref2)
     (_complete
      (make-residual-generator-ve* '_CELL-EQ?_MEMO (pred lv) ref1 ref2)))))

(define-syntax _make-vector_memo
  (syntax-rules ()
    ((_make-vector_memo 0 lab bt size arg)
     (static-vector lab size arg bt))
    ((_make-vector_memo lv lab bt size arg)
     (_complete
      (make-residual-generator-vvve* '_MAKE-VECTOR_MEMO (pred lv) 'lab (pred bt) size arg)))))

(define-syntax _message!_memo
  (syntax-rules ()
    ((_message!_memo 0 obj msg arg ...)
     ((obj 'msg) arg ...))
    ((_message!_memo lv obj msg arg ...)
     (_complete
      (make-residual-generator-veve* '_MESSAGE!_MEMO (pred lv) obj 'msg arg ...)))))

(define-syntax _if
  (syntax-rules ()
    ((_if 0 e1 e2 e3)
     (if e1 e2 e3))
;     ((_if 1 e1 e2 e3)
;      (shift k
; 	    (let* ((r1 e1)
; 		   (p2 (make-placeholder))
; 		   (p3 (make-placeholder))
; 		   (t2 (spawn
; 			(preserving-gensym-local
; 			 (lambda ()
; 			   (placeholder-set! p2
; 					     (with-fresh-meta-continuation
; 					      (lambda ()
; 						(reset (k e2)))))))))
; 		   (t3 (spawn
; 			(preserving-gensym-local
; 			 (lambda ()
; 			   (placeholder-set! p3
; 					     (with-fresh-meta-continuation
; 					      (lambda ()
; 						(reset (k e3))))))))))
; 	      (make-residual-if r1 (placeholder-value p2) (placeholder-value p3)))))
    ((_if 1 e1 e2 e3)
     (shift k (make-residual-if e1 (reset (k e2)) (reset (k e3)))))
    ((_if 1 e1 e2 e3)
     (shift k (let* ((cond-code e1)
		     (the-store (current-static-store!))
		     (then-code (reset (k e2)))
		     (xxxxxxxxx (install-static-store! the-store))
		     (else-code (reset (k e3))))
		(make-residual-if cond-code then-code else-code))))
    ((_if lv e1 e2 e3)
     (shift k (let* ((cond-code e1)
		     (the-store (current-static-store!))
		     (then-code (reset (k e2)))
		     (xxxxxxxxx (install-static-store! the-store))
		     (else-code (reset (k e3))))
		(make-residual-generator-ve*
		 '_IF (pred lv) cond-code then-code else-code))))))

(define-syntax _op
  (syntax-rules (apply cons _define_data _define)
    ((_op lv _define_data arg)
     (make-residual-define-data lv arg))
    ((_op lv _define var arg)
     (make-residual-define-mutable lv 'var arg))
    ((_op 0 op arg ...)
     (op arg ...))
    ((_op 1 cons e1 e2)
     (_complete (make-residual-cons e1 e2)))
    ((_op 1 apply f arg)
     (_complete-serious-apply f arg))
    ((_op 1 op arg ...)
     (_complete (make-residual-primop 'op arg ...)))
    ((_op lv op arg ...)
     (_complete (make-residual-generator-vve* '_OP (pred lv) 'op arg ...)))))

(define-syntax _op-no-result
  (syntax-rules (apply cons _define_data _define)
    ((_op-no-result lv _define_data arg)
     (make-residual-define-data lv arg))
    ((_op-no-result lv _define var arg)
     (make-residual-define-mutable lv 'var arg))
    ((_op-no-result 0 op arg ...)
     (op arg ...))
    ((_op-no-result 1 cons e1 e2)
     (_complete-no-result (make-residual-cons e1 e2)))
    ((_op-no-result 1 apply f arg)
     (_complete-serious-apply-no-result f arg))
    ((_op-no-result 1 op arg ...)
     (_complete-no-result (make-residual-primop 'op arg ...)))
    ((_op-no-result lv op arg ...)
     (_complete-no-result
      (make-residual-generator-vve* '_OP (pred lv) 'op arg ...)))))

(define-syntax _op-serious
  (syntax-rules (apply cons _define_data _define)
    ((_op-serious lv _define_data arg)
     (make-residual-define-data lv arg))
    ((_op-serious lv _define var arg)
     (make-residual-define-mutable lv 'var arg))
    ((_op-serious 0 op arg ...)
     (op arg ...))
    ((_op-serious 1 cons e1 e2)
     (_complete-serious 'cons (list e1 e2)))
    ((_op-serious 1 apply f arg)
     (_complete-serious-apply f arg))
    ((_op-serious 1 op arg ...)
     (_complete-serious 'op (list arg ...)))
    ((_op-serious lv op arg ...)
     (_complete (make-residual-generator-vve* '_OP-SERIOUS (pred lv) 'op arg ...)))))

(define-syntax _op-serious-no-result
  (syntax-rules (apply cons _define_data _define)
    ((_op-serious-no-result lv _define_data arg)
     (make-residual-define-data lv arg))
    ((_op-serious-no-result lv _define var arg)
     (make-residual-define-mutable lv 'var arg))
    ((_op-serious-no-result 0 op arg ...)
     (op arg ...))
    ((_op-serious-no-result 1 cons e1 e2)
     (_complete-serious-no-result 'cons (list e1 e2)))
    ((_op-serious-no-result 1 apply f arg)
     (_complete-serious-apply-no-result f arg))
    ((_op-serious-no-result 1 op arg ...)
     (_complete-serious-no-result 'op (list arg ...)))
    ((_op-serious-no-result lv op arg ...)
     (_complete-no-result
      (make-residual-generator-vve* '_OP-SERIOUS (pred lv) 'op arg ...)))))

(define-syntax _op_pure
  (syntax-rules (cons)
    ((_op_pure 0 op arg ...)
     (op arg ...))
    ((_op_pure 1 cons e1 e2)
     (make-residual-cons e1 e2))
    ((_op_pure 1 op arg ...)
     (make-residual-primop 'op arg ...))
    ((_op_pure lv op arg ...)
     (_complete
      (make-residual-generator-vve* '_OP_PURE (pred lv) 'op arg ...)))))

(define-syntax _freevar
  (syntax-rules ()
    ((_freevar 0 arg)
     arg)
;;;    ((_freevar 1 arg)
;;;     'arg)
    ((_freevar lv arg)
     (make-residual-generator-vve* '_FREEVAR (pred lv) 'arg))))

(define-syntax _lift0
  (syntax-rules ()
    ((_lift0 1 val)
     (make-residual-literal val))
    ((_lift0 lv val)
     (make-residual-generator-ve* '_LIFT0 (pred lv) val))))

(define-syntax _lift
  (syntax-rules ()
    ((_lift 0 diff value)
     (_lift0 diff value))
    ((_lift 1 diff value)
     (make-residual-generator-ve* '_LIFT0 'diff value))
    ((_lift lv diff value)
     (make-residual-generator-vve* '_LIFT (pred lv) 'diff value))))

(define-syntax _eval
  (syntax-rules ()
    ((_eval 0 0 body)
     (eval body (interaction-environment)))
    ((_eval 0 1 body)
     (_complete-maybe body))
    ((_eval 0 diff body)
     (_complete (make-residual-generator-vve* '_EVAL 0 (pred diff) body)))
    ((_eval 1 0 body)
     (_complete
      (make-residual-call 'EVAL body (make-residual-call 'INTERACTION-ENVIRONMENT))))
    ((_eval 1 1 body)
     body)				;;;?????????? _complete ??????????
    ((_eval lv diff body)
     (_complete
      (make-residual-generator-vve* '_EVAL (pred lv) 'diff body)))))

(define-syntax _run
  (syntax-rules ()
    ((_run 0 body)
     (eval (reset body) (interaction-environment)))
    ((_run l body)
     (_complete
      (make-residual-generator-ve* '_RUN (pred l) (reset body))))))
