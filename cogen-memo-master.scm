;;; Memoization master for distributed PE
;;;
;;; The master is the master process which controls the servers which
;;; perform the actual specialization work

(define-record pending-entry
  (program-point static-skeleton name bts fct))

(define *master-pending* #f)
(define *master-pending-lock* (make-lock))
(define *master-pending-placeholders*)
(define *master-pending-placeholders-lock* (make-lock))
(define (master-pending-initialize!)
  (set! *master-pending* (cons '***HEAD*** '()))
  (set! *master-pending-placeholders* '()))
(define (master-pending-add! key entry)
  (if (null? *master-pending-placeholders*)
      (set-cdr! *master-pending*
		(cons (cons key entry) (cdr *master-pending*)))
      (with-lock
       *master-pending-placeholders-lock*
       (lambda ()
	 (let ((placeholder (car *master-pending-placeholders*)))
	   (set! *master-pending-placeholders*
		 (cdr *master-pending-placeholders*))
	   (placeholder-set! placeholder entry))))))
(define (master-pending-lookup! key)
  (let loop ((previous *master-pending*)
	     (current (cdr *master-pending*)))
    (if (equal? key (caar current))
	(begin
	  (set-cdr! previous (cdr current))
	  (cdar current))
	(loop current (cdr current)))))
(define (master-pending-advance!)
  (if (null? (cdr *master-pending*))
      #f
      (let ((entry (cdadr *master-pending*)))
	(set-cdr! *master-pending* (cddr *master-pending*))
	entry)))
(define (master-pending-sign-up! placeholder)
  (with-lock
   *master-pending-placeholders-lock*
   (lambda ()
     (set! *master-pending-placeholders*
	   (cons placeholder *master-pending-placeholders*)))))

;;; the cache

;;; entries in the global cache
(define-record master-entry
  (program-point name ids+aspaces mark))

(define *master-cache* #f)
(define *master-cache-lock* (make-lock))
(define (master-cache-initialize!)
  (set! *master-cache* '()))
(define (master-cache-add! key entry)
  (set! *master-cache* (cons (cons key entry) *master-cache*)))
(define (master-cache-lookup key)
  (cond ((assoc key *master-cache*) => cdr)
	(else #f)))
(define (master-cache-lookup-by-local-id local-id final?)
  (let loop ((master-cache *master-cache*))
    (and (not (final? master-cache))
	 (let* ((master-entry (cdar master-cache))
		(ids+aspaces (master-entry->ids+aspaces master-entry)))
	   (or (and (assoc local-id ids+aspaces) master-entry)
	       (loop (cdr master-cache)))))))

;; messages sent from server to master

(define (server-working-on uid local-id) ; async
  ;; make sure that local-name is already registered with the master
  (if (not (can-server-work-on? uid local-id))
      (remote-run! (aspace-uid uid) server-kill-specialization local-id)))

(define (can-server-work-on? uid local-id)	; sync
    (display "Server ") (display uid) (display " asks if it can work on ") (display local-id) (newline)
  (let loop ((master-cache *master-cache*) (final? null?))
    (cond
     ((master-cache-lookup-by-local-id local-id final?)
      => (lambda (entry)
	   (master-entry->add-local-id! entry local-id uid)
	   (with-lock *master-cache-lock*
		      (lambda ()
			(if (master-entry->mark entry)
			    #f
			    (begin
			      (master-entry->mark! entry #t)
			      #t))))))
     (else				; wait until local name registered
      (relinquish-timeslice)
      (loop *master-cache* (lambda (item) (eq? item master-cache)))))))

(define (server-registers-memo-point! uid
				      program-point local-name local-id bts fct)
  (display "Server ") (display uid) (display " registers memo point ") (display program-point) (display ", local id ") (display local-id) (newline)
  (let ((static-skeleton (top-project-static program-point bts)))
    (cond
     ((master-cache-lookup static-skeleton)
      => (lambda (entry)
	   (master-entry->add-local-id! entry local-id (uid->aspace uid))))
     (else
      (begin
	(with-lock
	 *master-cache-lock*
	 (lambda ()
	   (master-cache-add!
	    static-skeleton
	    (make-master-entry program-point 			; key
			       local-name			; global name
			       (list (cons local-id (uid->aspace uid)))	; an alist 
			       #f)))))
	(with-lock
	 *master-pending-lock*
	 (lambda ()
	   (master-pending-add!
	    static-skeleton
	    (make-pending-entry program-point
				static-skeleton
				local-name
				bts
				fct))))))))
				

(define (server-is-unemployed uid)
  (display "Server ") (display uid) (display " says it's unemployed") (newline)
  (let ((entry (with-lock *master-pending-lock* master-pending-advance!)))
    (if entry
	;; we can put it right back to work
	(remote-run! (uid->aspace uid)
		     server-specialize
		     (pending-entry->name entry) 
		     (pending-entry->program-point entry)
		     (pending-entry->bts entry)
		     (pending-entry->fct entry))
	(let ((n-unemployed
	       (with-lock
		*n-unemployed-servers-lock*
		(lambda ()
		  (set! *n-unemployed-servers* (+ 1 *n-unemployed-servers*))
		  *n-unemployed-servers*))))
	  (if (= n-unemployed *n-servers*)
	      (display "Finished!")
	      (let ((entry-placeholder (make-placeholder)))
		(master-pending-sign-up! entry-placeholder)
		(let ((entry (placeholder-value entry-placeholder)))
		  (with-lock
		   *n-unemployed-servers-lock*
		   (lambda ()
		     (set! *n-unemployed-servers* (- *n-unemployed-servers* 1))))
		  (remote-run! (uid->aspace uid)
			       server-specialize
			       (pending-entry->name entry) 
			       (pending-entry->program-point entry)
			       (pending-entry->bts entry)
			       (pending-entry->fct entry)))))))))

;; Specialization driver

(define *server-aspaces* #f)
(define *n-servers* #f)
(define *n-unemployed-servers* #f)
(define *n-unemployed-servers-lock* (make-lock))

(define (start-specialization server-uids
			      level fname fct bts args)
    (master-cache-initialize!)
    (master-pending-initialize!)

    (set! *server-aspaces* (map uid->aspace server-uids))
    (set! *n-servers* (length *server-aspaces*))
    (set! *n-unemployed-servers* 0)

    (let* ((program-point (cons fname args))
	   (static-skeleton (top-project-static program-point bts))
	   (new-name (gensym fname)))
      (server-registers-memo-point!
       (car server-uids) program-point new-name #f bts fct))

    (for-each (lambda (aspace)
		(remote-run! aspace server-initialize! (local-aspace-uid)))
	      *server-aspaces*))

(define (collect-residual-program)
  (apply append
	 (map (lambda (aspace)
		(remote-apply aspace collect-local-residual-program))
	      *server-aspaces*)))

(define (master-entry->add-local-id! master-entry local-id uid)
  (let ((ids+aspaces (master-entry->ids+aspaces master-entry)))
    (master-entry->ids+aspaces!
     master-entry
     (cons (cons local-id uid) ids+aspaces))))
