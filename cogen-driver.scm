;;; cogen-driver.scm
;;; driver for the compiler generator

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

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
    (call-with-values
     (lambda ()
       (scheme-desugar full-source))
     (lambda (def-function* rejected*)
       ;; (writelpp def-function* "/tmp/def1.scm")
       (let* ((symbol-table (process-type-declarations rejected*))
	      (abssyn (scheme->abssyn-d def-function* symbol-table))
	      (d* (bta-run abssyn
			   symbol-table
			   skeleton
			   rejected*)))
	 (perform-termination-analysis d*)
	 (generate-d d*)
	 (append (filter (lambda (def) (eq? (car def) 'define-data))
			 rejected*)
		 *generating-extension*)))))) 
;;; TO DO:
;;; - error recognition & handling
;;; + remove Similix dependencies (i.e., file->list, anythingelse?) 
;;; + some support to run generating extensions, such as not having to
;;;   write (lambda (k) (k 5)) in place of 5 and avoiding awkwardness
;;;   when entering partially static data (it should be possible to
;;;   use (_CTOR_MEMO 1 'CTOR ...) in place of (CTOR ...) but this is
;;;   not really friendly ...)  
