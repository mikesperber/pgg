;;; Memoization master for distributed PE
;;;
;;; The master is the master process which controls the servers which
;;; perform the actual specialization work

(define-record pending-entry
  (program-point static-skeleton name bts fct server-uid local-id master-entry))

(define *master-pending* #f)
(define *master-pending-lock* #f)
(define *master-pending-placeholders* #f)
(define *master-pending-placeholders-lock* #f)

(define (master-pending-initialize!)
  (set! *master-pending* '())
  (set! *master-pending-lock* (make-lock))
  (set! *master-pending-placeholders* '())
  (set! *master-pending-placeholders-lock* (make-lock)))

(define (master-pending-add! key entry)
  (obtain-lock *master-pending-placeholders-lock*)
  (if (null? *master-pending-placeholders*)
      (begin
	(release-lock *master-pending-placeholders-lock*)
	(with-lock
	 *master-pending-lock*
	 (lambda ()
	   (set! *master-pending* (cons (cons key entry) *master-pending*)))))
      (let ((placeholder (car *master-pending-placeholders*)))
	(set! *master-pending-placeholders* (cdr *master-pending-placeholders*))
	(set! *n-unemployed-servers* (- *n-unemployed-servers* 1))
	(release-lock *master-pending-placeholders-lock*)
	(placeholder-set! placeholder entry))))

(define (master-pending-advance-no-locking!)
  (if (null? *master-pending*)
      #f
      (let ((entry (cdar *master-pending*)))
	(set! *master-pending* (cdr *master-pending*))
	entry)))

(define (master-pending-sign-up! placeholder)
  (obtain-lock *master-pending-placeholders-lock*)
  (set! *master-pending-placeholders*
	(cons placeholder *master-pending-placeholders*))
  (set! *n-unemployed-servers* (+ 1 *n-unemployed-servers*))
  (release-lock *master-pending-placeholders-lock*)
  (if (= *n-unemployed-servers* *n-servers*)
      (placeholder-set! *finished-placeholder* #t)))

;;; the cache

;;; entries in the global cache
(define-record master-entry
  (program-point name ids+aspaces lock mark))

(define *master-cache* #f)
(define *master-cache-ids* #f)
(define *master-cache-ids-size* 0)
(define *master-cache-lock* #f)

(define (master-cache-initialize!)
  (set! *master-cache* '())
  (set! *master-cache-ids-size* 501)
  (set! *master-cache-ids* (make-vector *master-cache-ids-size* '()))
  (set! *master-cache-lock* (make-lock)))

(define (master-cache-add-no-locking! key entry local-id)
  (set! *master-cache* (cons (cons key entry) *master-cache*))
  (let ((hash-key (modulo local-id *master-cache-ids-size*)))
    (vector-set! *master-cache-ids* hash-key
		 (cons (cons local-id entry)
		       (vector-ref *master-cache-ids* hash-key)))))

(define (master-cache-lookup key)
  (cond ((assoc key *master-cache*) => cdr)
	(else #f)))

(define (master-cache-delimited-lookup key final?)
  (let loop ((master-cache *master-cache*))
    (and (not (final? master-cache))
	 (if (equal? key (caar master-cache))
	     (cdar master-cache)
	     (loop (cdr master-cache))))))

(define (master-cache-lookup-by-local-id local-id)
  (let* ((hash-key (modulo local-id *master-cache-ids-size*))
	 (ids+entries (vector-ref *master-cache-ids* hash-key)))
    (cond ((assq local-id ids+entries)
	   => cdr)
	  (else #f))))

(define (master-entry->add-local-id! master-entry local-id uid)
  (with-lock
   (master-entry->lock master-entry)
   (lambda ()
     (let ((ids+aspaces (master-entry->ids+aspaces master-entry)))
       (master-entry->ids+aspaces!
	master-entry
	(cons (cons local-id uid) ids+aspaces)))))
  (with-lock
   *master-cache-lock*
   (lambda ()
     (let ((hash-key (modulo local-id *master-cache-ids-size*)))
       (vector-set! *master-cache-ids* hash-key
		    (cons (cons local-id master-entry)
			  (vector-ref *master-cache-ids* hash-key)))))))

;; messages sent from server to master

(define (server-working-on uid local-id) ; async
  ;; make sure that local-name is already registered with the master
  (if (not (can-server-work-on? uid local-id))
      'lose
      ;;(remote-run! (uid->aspace uid) server-kill-specialization! local-id)
      ))

;; this assumes that, if the answer is #t, the caller gets busy on entry!

(define (can-I-work-on-master-entry? entry)
  (with-lock
   (master-entry->lock entry)
   (lambda ()
     (if (master-entry->mark entry)
	 #f
	 (begin
	   (master-entry->mark! entry #t)
	   #t)))))

(define (kill-master-entry-except-on! entry uid)
  (for-each
   (lambda (id+aspace)
     (let* ((id (car id+aspace))
	    (kill-uid (aspace-uid (cdr id+aspace)))
	    (p (vector-ref *servers-killed* kill-uid)))
       (if (not (= kill-uid uid))
	   (with-lock
	    (car p)
	    (lambda ()
	      (set-cdr! p (cons id (cdr p))))))))
   (master-entry->ids+aspaces entry)))

(define (can-server-work-on? uid local-id) ; sync
  (let loop ()
    ;; (display (list "can-server-work-on?" uid local-id)) (newline)
    (let ((placeholder (vector-ref *servers-placeholders* uid)))
      (cond
       ((master-cache-lookup-by-local-id local-id)
	=> (lambda (entry)
	     (let ((can-I? (can-I-work-on-master-entry? entry)))
	       (if can-I?
		   (kill-master-entry-except-on! entry uid))
	       (let* ((p (vector-ref *servers-killed* uid))
		      (lock (car p)))
		 (obtain-lock lock)
		 (let ((killed (cdr p)))
		  (set-cdr! p '())
		  (release-lock lock)
		  (values can-I? killed))))))
       (else				; wait until local name registered
	;; (display "Server ") (display uid) (display " suspends on: ")(display (list "can-server-work-on?" local-id)) (newline)
	(placeholder-value placeholder)
	;; (display "Server ") (display uid) (display " again: ")(display (list "can-server-work-on?" local-id)) (newline)
	(loop))))))

;;; receives wrapped program-points
(define (server-registers-memo-point! uid
				      program-point local-name local-id bts fct)
  ;; (display "Server ") (display uid) (display " registers memo point ")
  ;; (display program-point)
  ;; (display ", local id ") (display local-id) (newline)
  (let ((set-placeholders
	 (lambda ()
	   (if uid
	       (begin
		 ;; (display (list "Server" uid local-id "setting placeholder")) (newline)
		 (with-lock
		  *servers-placeholders-lock*
		  (lambda ()
		    (let ((placeholder (vector-ref *servers-placeholders* uid)))
		      (placeholder-set! placeholder #t)
		      (vector-set! *servers-placeholders* uid (make-placeholder)))))
		 ;; (display (list "Server" uid local-id "setting placeholder done")) (newline)
		 ))))
	(static-skeleton (top-project-static (unwrap-program-point program-point) bts))
	(master-cache *master-cache*))
      (cond
       ((master-cache-lookup static-skeleton)
	=> (lambda (entry)
	     ;; (display (list "Server" uid "found memo point" local-id)) (newline)
	     (master-entry->add-local-id! entry local-id (uid->aspace uid))
	     (set-placeholders)))
       (else
	(let* ((master-entry
		(make-master-entry program-point ; key
				   local-name ; global name
				   (list (cons local-id (uid->aspace uid))) ; an alist
				   (make-lock)
				   #f))
	       (pending-entry
		(make-pending-entry program-point
				    static-skeleton
				    local-name
				    bts
				    fct
				    uid local-id
				    master-entry)))

	  (obtain-lock *master-cache-lock*)
	  (cond ((master-cache-delimited-lookup
		  static-skeleton
		  (lambda (item) (eq? item master-cache)))
		 => (lambda (entry)
		      (release-lock *master-cache-lock*)
		      (master-entry->add-local-id! entry local-id (uid->aspace uid))))
		(else
		 ;; we can go ahead
		 ;; order is important here, no?
		 (master-cache-add-no-locking! static-skeleton master-entry local-id)
		 (release-lock *master-cache-lock*)
		 (set-placeholders)
		 (master-pending-add! static-skeleton pending-entry)))))))

  ;; (display "Registration of local id ") (display local-id) (display " complete") (newline)
  )


(define (server-is-unemployed uid)
  ;; (display "Server ") (display uid) (display " says it's unemployed") (newline)
  (let loop ()
    (let ((entry (with-lock
		  *master-pending-lock*
		  (lambda ()
		    (master-pending-advance-no-locking!)))))
      (if entry
	  (if (can-I-work-on-master-entry? (pending-entry->master-entry entry))
	      (begin
		(kill-master-entry-except-on! (pending-entry->master-entry entry) uid)
		;; we can put it right back to work
		(remote-run! (uid->aspace uid)
			     server-specialize ;; -async
			     (pending-entry->name entry) 
			     (pending-entry->program-point entry)
			     (pending-entry->bts entry)
			     (pending-entry->fct entry)))
	      ;; Don't call us, we'll call you.  Next, please!
	      (loop))
	  (let ((entry-placeholder (make-placeholder)))
	    (master-pending-sign-up! entry-placeholder)
	    (let ((entry (placeholder-value entry-placeholder)))
	      (kill-master-entry-except-on! (pending-entry->master-entry entry) uid)
	      (remote-run! (uid->aspace uid)
			   server-specialize ;; -async
			   (pending-entry->name entry) 
			   (pending-entry->program-point entry)
			   (pending-entry->bts entry)
			   (pending-entry->fct entry))))))))

;; Specialization driver

(define *servers-placeholders* #f)
(define *servers-placeholders-lock* #f)
(define *server-aspaces* #f)
(define *n-servers* #f)
(define *n-unemployed-servers* #f)
(define *n-unemployed-servers-lock* #f)

(define *servers-killed* #f)

(define *finished-placeholder* #f)

(define (servers-placeholders-initialize!)
  (set! *servers-placeholders*
	(make-vector (+ 1 (apply max
				 (map aspace-uid *server-aspaces*)))
		     #f))
  (set! *servers-placeholders-lock* (make-lock))
  ;; sort of a kludge
  (for-each
   (lambda (aspace)
     (vector-set! *servers-placeholders* (aspace-uid aspace) (make-placeholder)))
   *server-aspaces*))

(define (servers-killed-initialize!)
  (set! *servers-killed*
	(make-vector (+ 1 (apply max
				 (map aspace-uid *server-aspaces*)))
		     #f))
  (for-each
   (lambda (aspace)
     (vector-set! *servers-killed* (aspace-uid aspace) (cons (make-lock) '())))
   *server-aspaces*))

(define (start-specialization server-uids
			      level fname fct bts args)
    (set! *server-aspaces* (map uid->aspace server-uids))
    (set! *n-servers* (length *server-aspaces*))
    (set! *n-unemployed-servers* 0)
    (set! *n-unemployed-servers-lock* (make-lock))

    (master-cache-initialize!)
    (master-pending-initialize!)
    (servers-placeholders-initialize!)
    (servers-killed-initialize!)

    (let* ((program-point (wrap-program-point (cons fname args) bts))
	   (new-name (gensym fname)))
      (server-registers-memo-point!
       (car server-uids) program-point new-name 0 bts fct))

    (set! *finished-placeholder* (make-placeholder))
    (display "starting remote initialization") (newline)
    (for-each (lambda (aspace)
		(remote-run! aspace server-initialize! (local-aspace-uid)))
	      *server-aspaces*)
    (placeholder-value *finished-placeholder*))

(define (collect-residual-program)
  (apply append
	 (map (lambda (aspace)
		(remote-apply aspace collect-local-residual-program))
	      *server-aspaces*)))

(define (display-kill-counts)
  (for-each
   (lambda (aspace)
     (display "Aspace #") (display (aspace-uid aspace)) (display ": ")
     (display (remote-apply aspace get-local-kill-count))
     (newline))
   *server-aspaces*))
