;;; finite functions

(define-record empty-ff ())
(define-record extended-ff (key val ff))
(define-record extended-ff* (key* val* ff))

(define new-ff make-empty-ff)
(define extend-ff make-extended-ff)
(define extend-ff*
  (lambda (key* val* ff)
    (make-extended-ff* key* val* ff)))
(define apply-ff
  (lambda (ff key fail)
    (let loop ((ff ff))
      (cond
       ((empty-ff? ff)
	(fail))
       ((extended-ff? ff)
	(if (equal? key (extended-ff->key ff))
	    (extended-ff->val ff)
	    (loop (extended-ff->ff ff))))
       ((extended-ff*? ff)
	(ribassoc-c key
		    (extended-ff*->key* ff)
		    (extended-ff*->val* ff)
		    (lambda () (loop (extended-ff*->ff ff)))))
       (else
	(error "apply-ff: finite function expected"))))))
(define ribassoc-c
  (lambda (key key* val* fail)
    (let loop ((key* key*) (val* val*))
      (if (null? key*)
	  (fail)
	  (if (equal? (car key*) key)
	      (car val*)
	      (loop (cdr key*) (cdr val*)))))))

(define map-ff
  (lambda (fun ff)
    (let loop ((ff ff))
      (cond
       ((empty-ff? ff)
	(make-empty-ff))
       ((extended-ff? ff)
	(extend-ff (extended-ff->key ff)
		   (fun (extended-ff->val ff))
		   (loop (extended-ff->ff ff))))
       ((extended-ff*? ff)
	(extend-ff* (extended-ff*->key* ff)
		    (map fun (extended-ff*->val* ff))
		    (loop (extended-ff*->ff ff))))
       (else
	(error "map-ff: finite function expected"))))))

(define for-each-ff!
  (lambda (proc! ff)
    (let loop ((ff ff))
      (cond
       ((empty-ff? ff)
	'done)
       ((extended-ff? ff)
	(proc! (extended-ff->val ff))
	(loop (extended-ff->ff ff)))
       ((extended-ff*? ff)
	(for-each proc! (extended-ff*->val* ff))
	(loop (extended-ff*->ff ff)))
       (else
	(error "map-ff: finite function expected"))))))

;;; environments

(define the-empty-env (new-ff))
(define extend-env extend-ff)
(define extend-env* extend-ff*)
(define apply-env apply-ff)
(define map-env map-ff)
(define for-each-env! for-each-ff!)