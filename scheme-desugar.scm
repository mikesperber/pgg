;;; scheme-desugar

(define *macro-source*
  (file->list
   (namestring "scheme-standard-macros.scm"
	       (file-name-directory (%file-name%))
	       #f)))

(define (desugar job-file/files)
  (let* ((source-files
	  (if (string? job-file/files)
	      (map symbol->string (file->list job-file/files))
	      job-file/files))
	 (full-source
	  (cons `(BEGIN ,@*macro-source*)
		(map (lambda (filename)
		       `(BEGIN ,@(file->list filename)))
		     source-files))))
    (scheme-desugar full-source)))
