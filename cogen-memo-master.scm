;;; Memoization master for distributed PE
;;;
;;; The master is the master process which controls the servers which
;;; perform the actual specialization work

(define *servers-placeholders* #f)
(define *servers-placeholders-lock* #f)
(define *server-aspaces* #f)
(define *n-servers* #f)
(define *n-idle-servers* #f)
(define *n-idle-servers-lock* #f)

(define *servers-killed* #f)

(define *finished-placeholder* #f)

(define-record pending-root
  (lock entries killed-local-ids run-time-table))
(define-record pending-entry
  (program-point static-skeleton name bts fct
   server-uid local-id
   master-entry))
(define-record run-time-entry
  (n-specializations average-elapsed-time lock))

(define *master-pending-by-uid* #f)
(define *master-pending-placeholders* #f)
(define *master-pending-placeholders-lock* #f)

(define (master-pending-initialize!)
  (set! *master-pending-by-uid*
	(make-vector (+ 1 (apply max
				 (map aspace-uid *server-aspaces*)))
		     #f))
  (for-each
   (lambda (aspace)
     (vector-set! *master-pending-by-uid* (aspace-uid aspace)
		  (make-pending-root (make-lock) '() '() (make-symbol-table))))
   *server-aspaces*)
  (set! *master-pending-placeholders* '())
  (set! *master-pending-placeholders-lock* (make-lock)))

(define (master-pending-add! uid key entry)
  (obtain-lock *master-pending-placeholders-lock*)
  (if (null? *master-pending-placeholders*)
      (begin
	(release-lock *master-pending-placeholders-lock*)
	(let ((pending-root (vector-ref *master-pending-by-uid* uid)))
	  (with-lock
	   (pending-root->lock pending-root)
	   (lambda ()
	     (pending-root->entries!
	      pending-root
	      (cons entry (pending-root->entries pending-root)))))))
      (let ((placeholder (car *master-pending-placeholders*)))
	(set! *master-pending-placeholders* (cdr *master-pending-placeholders*))
	(set! *n-idle-servers* (- *n-idle-servers* 1))
	(release-lock *master-pending-placeholders-lock*)
	(placeholder-set! placeholder entry))))

(define (uid-pending-advance! uid)
  (let* ((pending-root (vector-ref *master-pending-by-uid* uid))
	 (lock (pending-root->lock pending-root)))
    (obtain-lock lock)
    (let ((entries (pending-root->entries pending-root)))
      (if (null? entries)
	  (begin
	    (release-lock lock)
	    #f)
	  (let ((entry (car entries)))
	    (pending-root->entries! pending-root (cdr entries))
	    (release-lock lock)
	    entry)))))

(define (master-pending-advance!)
  (let loop ((aspaces *server-aspaces*))
    (and (not (null? aspaces))
	 (or (uid-pending-advance! (aspace-uid (car aspaces)))
	     (loop (cdr aspaces))))))

  
(define (master-pending-sign-up! placeholder)
  (obtain-lock *master-pending-placeholders-lock*)
  (set! *master-pending-placeholders*
	(cons placeholder *master-pending-placeholders*))
  (set! *n-idle-servers* (+ 1 *n-idle-servers*))
  (release-lock *master-pending-placeholders-lock*)
  (if (= *n-idle-servers* *n-servers*)
      (placeholder-set! *finished-placeholder* #t)))

;;; the cache

;;; entries in the global cache
(define-record master-entry
  (program-point name ids+aspaces lock mark arrival-time))

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
	    (root (vector-ref *master-pending-by-uid* kill-uid)))
       (if (not (eqv? kill-uid uid))
	   (with-lock
	    (pending-root->lock root)
	    (lambda ()
	      (pending-root->killed-local-ids!
	       root (cons id (pending-root->killed-local-ids root))))))))
   (master-entry->ids+aspaces entry)))

(define (can-server-work-on uid local-id)
  (let loop ()
    ;; (display (list "can-server-work-on" uid local-id)) (newline)
    (let ((placeholder (vector-ref *servers-placeholders* uid)))
      (cond
       ((master-cache-lookup-by-local-id local-id)
	=> (lambda (entry)
	     (let ((can-I? (can-I-work-on-master-entry? entry)))
	       (if can-I?
		   (kill-master-entry-except-on! entry uid))
	       (let* ((root (vector-ref *master-pending-by-uid* uid))
		      (lock (pending-root->lock root)))
		 (obtain-lock lock)
		 (let ((killed (pending-root->killed-local-ids root)))
		   (pending-root->killed-local-ids! root '())
		   (release-lock lock)
		   (if can-I?
		       (values can-I? killed)
		       (values (uid-pending-advance! uid) killed)))))))
						   
       (else				; wait until local name registered
	;; (display "Server ") (display uid) (display " suspends on: ")(display (list "can-server-work-on?" local-id)) (newline)
	(placeholder-value placeholder)
	;; (display "Server ") (display uid) (display " again: ")(display (list "can-server-work-on" local-id)) (newline)
	(loop))))))

;;; receives wrapped program-points
(define (server-registers-memo-point! uid async?
				      program-point local-name local-id bts fct)
  ;; (display "Server ") (display uid) (display " registers memo point ")
  ;; (display program-point)
  ;; (display ", local id ") (display local-id) (newline)
  (let ((arrival-time (real-time))
	(set-placeholders
	 (lambda ()
		 ;; (display (list "Server" uid local-id "setting placeholder")) (newline)
		 (with-lock
		  *servers-placeholders-lock*
		  (lambda ()
		    (let ((placeholder (vector-ref *servers-placeholders* uid)))
		      (placeholder-set! placeholder #t)
		      (vector-set! *servers-placeholders* uid (make-placeholder)))))
		 ;; (display (list "Server" uid local-id "setting placeholder done")) (newline)
		 ))
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
				   #f
				   arrival-time))
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
		 (master-pending-add! uid static-skeleton pending-entry)))))))

  ;; (display "Registration of local id ") (display local-id) (display " complete") (newline)
  )

(define (register-initial-memo-point! program-point local-name local-id bts fct)
  (let* ((static-skeleton (top-project-static (unwrap-program-point program-point) bts))
	 (master-entry
	  (make-master-entry program-point ; key
			     local-name ; global name
			     '()
			     (make-lock)
			     #f
			     (real-time)))
	 (pending-entry
	  (make-pending-entry program-point
			      static-skeleton
			      local-name
			      bts
			      fct
			      #f local-id
			      master-entry)))

    (master-cache-add-no-locking! static-skeleton master-entry local-id)
    (master-pending-add! (aspace-uid (car *server-aspaces*))
			 static-skeleton pending-entry)))


(define (server-working-on uid local-id)
  ;; (display ">>>server-working-on ") (display local-id) (newline)
  (let loop ()
    ;; (display (list "server-working-on" uid local-id)) (newline)
    (let ((placeholder (vector-ref *servers-placeholders* uid)))
      (cond
       ((master-cache-lookup-by-local-id local-id)
	=> (lambda (entry)

	     (master-entry->mark! entry #t)

	     (let* ((process-time (real-time))
		    (root (vector-ref *master-pending-by-uid* uid))
		    (lock (pending-root->lock root))
		    (table (pending-root->run-time-table root))
		    (memo-point (car (master-entry->program-point entry))))
	       (obtain-lock lock)
	       (let ((time-entry (or (table-ref table memo-point)
				     (let ((time-entry (make-run-time-entry 0 0 (make-lock))))
				       (table-set! table memo-point time-entry)
				       time-entry))))
		 (release-lock lock)
		 (with-lock
		  (run-time-entry->lock time-entry)
		  (lambda ()
		    (let ((n-specializations 
			   (run-time-entry->n-specializations time-entry)))
		      (run-time-entry->average-elapsed-time!
		       time-entry
		       (quotient
			(+
			 (* n-specializations
			    (run-time-entry->average-elapsed-time time-entry))
			 (- process-time (master-entry->arrival-time entry)))
			(+ 1 n-specializations)))
		      (run-time-entry->n-specializations!
		       time-entry (+ 1 n-specializations)))))))
	       
	     ;; Hopefully, latencies will be such that the above runs
	     ;; in little time compared to the time it takes to send a
	     ;; message.  Generally if course: If you must kill, do it
	     ;; quick.  --Mike
	     
	     (for-each
	      (lambda (id+aspace)
		(let* ((aspace (cdr id+aspace))
		       (kill-uid (aspace-uid aspace))
		       (id (car id+aspace)))
		  (if (not (eqv? kill-uid id))
		      (remote-run! aspace
				   server-kill-local-id!
				   id))))
	      (master-entry->ids+aspaces entry))))
       (else				; wait until local name registered
	;; (display "Server ") (display uid) (display " suspends on: ")(display (list "server-working-on" local-id)) (newline)
	(placeholder-value placeholder)
	;; (display "Server ") (display uid) (display " again: ")(display (list "server-working-on" local-id)) (newline)
	(loop))))))

(define (server-is-idle uid async?)
  ;; (display "Server ") (display uid) (display " says it's idle, async") (display async?) (newline)
  (let loop ()
    (let ((entry (master-pending-advance!)))
      (if entry
	  (if (and (can-I-work-on-master-entry? (pending-entry->master-entry entry))
		   ;; this is safe: if the server is idle, it must
		   ;; have done the job already
		   (not (eqv? (pending-entry->server-uid entry) uid)))
	      (begin
		(kill-master-entry-except-on! (pending-entry->master-entry entry) uid)
		;; we can put it right back to work
		(remote-run! (uid->aspace uid)
			     (if async?
				 server-specialize-async
				 server-specialize)
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
			   (if async?
			       server-specialize-async
			       server-specialize)
			   (pending-entry->name entry) 
			   (pending-entry->program-point entry)
			   (pending-entry->bts entry)
			   (pending-entry->fct entry))))))))

;; Specialization driver

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

(define (start-specialization server-uids async?
			      level fname fct bts args)
    (set! *server-aspaces* (map uid->aspace server-uids))
    (set! *n-servers* (length *server-aspaces*))
    (set! *n-idle-servers* 0)
    (set! *n-idle-servers-lock* (make-lock))

    (master-cache-initialize!)
    (master-pending-initialize!)
    (servers-placeholders-initialize!)

    (let* ((program-point (wrap-program-point (cons fname args) bts))
	   (new-name (gensym fname)))
      (register-initial-memo-point! program-point new-name 0 bts fct))

    (set! *finished-placeholder* (make-placeholder))
    (display "starting remote initialization") (newline)
    (for-each (lambda (aspace)
		(remote-run! aspace server-initialize! (local-aspace-uid) async?))
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

(define (display-elapsed-times)
  (for-each
   (lambda (aspace)
     (display "Aspace #") (display (aspace-uid aspace)) (newline)
     (let* ((uid (aspace-uid aspace))
	    (root (vector-ref *master-pending-by-uid* uid))
	    (run-time-table (pending-root->run-time-table root)))
       (table-walk
	(lambda (key time-entry)
	  (display key) (display " --- ")
	  (display (run-time-entry->n-specializations time-entry))
	  (display "x, ")
	  (display (run-time-entry->average-elapsed-time time-entry))
	  (display " ms") (newline))
	run-time-table)))
   *server-aspaces*))
