;;; driver for the compiler generator

;;; the main entry point
;;; `job-file' contains a list of file names which contain the source
;;; code and declarations for the current project
;;; `skeleton' is a prototype call with arguments replaced by their
;;; binding times 
;;; proposal for specifying partially static input: a list containing
;;; bt-prototypes for the expected constructors, e.g.:
;;; '((anil) (acons d *)) means "expect a list with static spine
;;; and dynamic elements"; it is a shorthand for the recursive type 
;;; \mu \alpha . ANIL + CONS d \alpha.
(define (cogen-driver job-file skeleton)
  (let* ((source-files
	  (map symbol->string (file->list job-file)))
	 (full-source
	  (apply append (map file->list source-files)))
	 (def-function*
	   (filter (lambda (defn) (equal? (car defn) 'define))
		   full-source))
	 (def-datatype*
	   (filter (lambda (defn) (equal? (car defn) 'define-type))
		   full-source))
	 (d*
	  (bta-run (scheme->abssyn-d def-function*) def-datatype* skeleton)))
    (generate-d d*)
    *generating-extension*)) 
;;; TO DO:
;;; - error recognition & handling
;;; - remove Similix dependencies (i.e., file->list, anythingelse?) 
;;; - some support to run generating extensions, such as not having to
;;;   write (lambda (k) (k 5)) in place of 5 and avoiding awkwardness
;;;   when entering partially static data (it should be possible to
;;;   use (_CTOR_MEMO 1 'CTOR ...) in place of (CTOR ...) but this is
;;;   not really friendly ...)  