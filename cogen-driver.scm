;;; driver for the compiler generator

;;; the main entry point
;;; `job-file' contains a list of file names which contain the source
;;; code and declarations for the current project (can also be the
;;; list of filenames itself)
;;; `skeleton' is a prototype call with arguments replaced by their
;;; binding times 
;;; proposal for specifying partially static input: a list containing
;;; bt-prototypes for the expected constructors, e.g.:
;;; '((anil) (acons d *)) means "expect a list with static spine
;;; and dynamic elements"; it is a shorthand for the recursive type 
;;; \mu \alpha . ANIL + CONS d \alpha.

(define *macro-source*
  (file->list
   (namestring "scheme-standard-macros.scm"
	       (file-name-directory (%file-name%))
	       #f)))

(define (cogen-driver job-file/files skeleton)
  (let* ((source-files
	  (if (string? job-file/files)
	      (map symbol->string (file->list job-file/files))
	      job-file/files))
	 (full-source
	  (append *macro-source*
		  (apply append (map file->list source-files)))))
    (let loop ((D* full-source)
	       (def-function* '())
	       (def-type*     '())
	       (def-syntax*   '())
	       (other*	      '()))
      (if (pair? D*)
	  (let ((D (car D*))
		(D* (cdr D*)))
	    (case (car D)
	      ((define define-without-memoization)
	       (loop D* (cons D def-function*) def-type* def-syntax* other*))
	      ((define-data define-type define-primitive define-memo)
	       (loop D* def-function* (cons D def-type*) def-syntax* other*))
	      ((define-syntax)
	       (loop D* def-function* def-type* (cons D def-syntax*) other*))
	      (else
	       (loop D* def-function* def-type* def-syntax* (cons D other*)))))
	  ;; finally:
	  (let* ((symbol-table (process-type-declarations def-type*))
		 (preprocessed-source (scheme->abssyn-d def-function*
							def-syntax*
							other*
							symbol-table))
		 (d* (bta-run preprocessed-source
			      symbol-table
			      skeleton
			      def-type*)))
	    (perform-termination-analysis d*)
	    (generate-d d*)
	    (append (filter (lambda (def) (eq? (car def) 'define-data))
			    def-type*)
		    *generating-extension*)))))) 
;;; TO DO:
;;; - error recognition & handling
;;; + remove Similix dependencies (i.e., file->list, anythingelse?) 
;;; + some support to run generating extensions, such as not having to
;;;   write (lambda (k) (k 5)) in place of 5 and avoiding awkwardness
;;;   when entering partially static data (it should be possible to
;;;   use (_CTOR_MEMO 1 'CTOR ...) in place of (CTOR ...) but this is
;;;   not really friendly ...)  
