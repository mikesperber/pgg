;;; introspective memoization
;;; suppose the following functinos are provided by an implementation

;;; reify-closure procedure

;;; when applied to a procedure, reify-closure returns a closure


(define (project-static value bts)
  (let ((ctor (car value))
        (values (cdr value)))
    (cons ctor
          (let loop ((values values) (bts bts))
            (if (null? values)
                '()
                (let ((skeleton (loop (cdr values) (cdr bts))))
                  (if (= 0 (car bts))
                      (let ((static-value (car values)))
                        (cond
			 ((procedure? static-value)
			  (append (project-static-proc static-value)
				  skeleton))
			 ((constructor? static-value)
			  (append (project-static-ctor static-value)
				  skeleton))
			 (else
                            (cons static-value skeleton))))
                      skeleton)))))))

(define (project-static-proc proc)
  (let ((closure (reify-closure proc)))
    (let ((freevals (closure->vals closure)))
      (project-static (cons (closure->label closure)
			    (map binding->value freevals))
		      (map binding->btime freevals)))))

(define (project-static-ctor cval)
  (project-static (cons (constructor->ctor cval)
			(constructor->args cval))
		  (constructor->bts  cval)))


;;;;;;

(define (clone-dynamic value bts)
  (cons (car value)
	(let loop ((values (cdr value)) (bts bts))
	  (if (null? values)
	      '()
	      (let ((skeleton (loop (cdr values) (cdr bts))))
		(if (= 0 (car bts))
		    (let ((static-value (car values)))
		      (cond
		       ((procedure? static-value)
			(cons (clone-dynamic-proc static-value)
			      skeleton))
		       ((constructor? static-value)
			(append (clone-dynamic-ctor static-value)
				skeleton))
			 (else
                          (cons static-value skeleton))))
		    (cons (gensym-local 'clone) skeleton)))))))

(define (clone-dynamic-proc proc)
  (let* ((closure (reify-closure proc))
	 (label (closure->label closure))
	 (freevals (closure->vals closure))
	 (cloned-freevals (cdr (clone-dynamic
				(cons label
				      (map binding->value freevals))
				(map binding->btime freevals)))))
    (reflect-closure
     (make-closure label cloned-freevals))))

(define (clone-dynamic-ctor cval)
  (let ((ctor (constructor->ctor cval)))
    (make-constructor
     ctor
     (cdr (clone-dynamic ctor
			 (constructor->args cval)
			 (constructor->bts cval)))
     (constructor->bts cval))))
