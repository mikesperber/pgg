; Mixwell interpreter
; Copyright (C) 1993 Anders Bondorf

;-----------------------------------------------------------------------------
; P ::= (D1 D2 ... Dn)
; D ::= (F (V1 ... Vn) = E)
; E ::= V | (quote C)
;     | (car E) | (cdr E) | (atom E) | (cons E E) | (equal E E)
;     | (if E E E) | (call F E1 ... En)

;------------------------------------------------------------------------------

(define-data binding
  (make-binding binding-name binding-value))

(define-data bindings
  (bindings-nil)
  (bindings-cons bindings-car bindings-cdr))

(define-primitive _error - error)

;-----------------------------------------------------------------------------

;------------------------------------------------------------------------------

(define (run-mixwell p vals)
  (let ((vs (cadr (car p)))
	(e (cadddr (car p))))
    (ev e
	(let ((arity (length vs)))
	  (let loop ((i 0))
	    (if (= i arity)
		(init-env)
		(upd-env (list-ref vs i)
			 (list-ref vals i)
			 (loop (+ 1 i))))))
	p)))

(define (ev e r p)
  (if (or (symbol? e)
	  (boolean? e)
	  (number? e))
      (lookup-env e r)			; e = variable v
      (let ((op (car e)))
	(cond
	 ((or (eq? 'quote op) (eq? 'generalize op)) (cadr e))
	 ((eq? 'car op) (car (ev (cadr e) r p)))
	 ((eq? 'cdr op) (cdr (ev (cadr e) r p)))
	 ((eq? 'atom op) (not (pair? (ev (cadr e) r p))))
	 ((eq? 'pair? op) (pair? (ev (cadr e) r p)))
	 ((eq? 'null? op) (null? (ev (cadr e) r p)))
	 ((eq? 'cons op)
	  (cons (ev (cadr e) r p) (ev (caddr e) r p)))
	 ((eq? 'equal? op)
	  (equal? (ev (cadr e) r p) (ev (caddr e) r p)))
	 ((eq? 'if op)
	  (if (ev (cadr e) r p)
	      (ev (caddr e) r p)
	      (ev (cadddr e) r p)))
	 ((or (eq? 'call op)
	      (eq? 'xcall op)
	      (eq? 'rcall op))
	  (let ((d (assoc (cadr e) p)))
	    (let ((f (car d))
		  (vs (cadr d)))
	      (ev (cadddr d)
		  (let loop ((vs vs) (es (cddr e)))
		    (if (null? vs)
			(init-env)
			(upd-env (car vs)
				 (ev (car es) r p)
				 (loop (cdr vs) (cdr es)))))
		  p))))
	 (else
	  (_error "illegal expression syntax"))))))

(define (init-env) (bindings-nil))
(define (upd-env V val r) (bindings-cons (make-binding V val) r))

(define (lookup-env V r)
  (let loop ((bs r))
    (if (bindings-nil? bs)
	(_error "Name ~s not bound")
	(let ((binding (bindings-car bs)))
	  (if (equal? V (binding-name binding))
	      (binding-value binding)
	      (loop (bindings-cdr bs)))))))

;------------------------------------------------------------------------------
