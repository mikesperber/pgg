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
(define (cogen-driver job-file/files skeleton)
  (let* ((source-files
	  (if (string? job-file/files)
	      (map symbol->string (file->list job-file/files))
	      job-file/files))
	 (full-source
	  (apply append (map file->list source-files)))
	 (def-function*
	   (filter (lambda (defn) (equal? (car defn) 'define))
		   full-source))
	 (def-datatype*
	   (filter (lambda (defn) (equal? (car defn) 'define-data))
		   full-source))
	 (def-typesig*
	   (filter (lambda (defn) (equal? (car defn) 'define-type))
		   full-source))
	 (def-opsig*
	   (filter (lambda (defn) (equal? (car defn) 'define-primitive))
		   full-source))
	 (def-memo (assoc 'define-memo full-source))
	 (def-opsig*
	   (if def-memo
	       (cons `(DEFINE-OPERATOR ,(cadr def-memo) MEMO) def-typesig*)
	       def-opsig*))
	 (symbol-table
	  (scheme->abssyn-define-type def-datatype* def-typesig* def-opsig*))
	 (d*
	  (bta-run (scheme->abssyn-d def-function* symbol-table)
		   symbol-table
		   skeleton
		   def-typesig*
		   def-opsig*
		   def-memo)))
    (generate-d d*)
    (append def-datatype*
	    *generating-extension*))) 
;;; TO DO:
;;; - error recognition & handling
;;; + remove Similix dependencies (i.e., file->list, anythingelse?) 
;;; + some support to run generating extensions, such as not having to
;;;   write (lambda (k) (k 5)) in place of 5 and avoiding awkwardness
;;;   when entering partially static data (it should be possible to
;;;   use (_CTOR_MEMO 1 'CTOR ...) in place of (CTOR ...) but this is
;;;   not really friendly ...)  