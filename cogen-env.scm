;;; cogen-env.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

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

(define env-tag
  '(env))
(define (is-env? x)
  (and (pair? x) (eq? (car x) env-tag)))
(define (unbox-env x)
  (if (is-env? x)
      (cdr x)
      (error "unbox-env: not a boxed environment" x)))
(define (setbox-env! x y)
  (if (is-env? x)
      (set-cdr! x y)
      (error "setbox-env!: not a boxed environment" x)))
(define (make-boxed-env env)
  (cons env-tag env))
(define (empty-boxed-env)
  (make-boxed-env the-empty-env))
(define (shrink-boxed-env env)
  (let ((ff (unbox-env env)))
    (setbox-env!
     env
     (cond
      ((empty-ff? ff)
       (error "shrink-boxed-env: env is empty"))
      ((extended-ff? ff)
       (extended-ff->ff ff))
      ((extended-ff*? ff)
       (extended-ff*->ff ff))
      (else
       (error "shrink-boxed-env: finite function expected"))))))
(define (extend-boxed-env key val env)
  (setbox-env! env (extend-env key val (unbox-env env)))
  env)
(define (extend-boxed-env* key* val* env)
  (setbox-env! env (extend-env* key* val* (unbox-env env)))
  env)
(define (fresh-boxed-env* key* val* env)
  (make-boxed-env (extend-env* key* val* (unbox-env env))))
(define (apply-boxed-env env key fail)
  (apply-env (unbox-env env) key fail))
