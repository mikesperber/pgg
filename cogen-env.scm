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
	(error "apply-ff: expected finite function"))))))

(define ribassoc-c
  (lambda (key key* val* fail)
    (let loop ((key* key*) (val* val*))
      (if (null? key*)
	  (fail)
	  (if (equal? (car key*) key)
	      (car val*)
	      (loop (cdr key*) (cdr val*)))))))

;;; environments

(define the-empty-env (new-ff))
(define extend-env extend-ff)
(define extend-env* extend-ff*)
(define apply-env apply-ff)
