;;; toplevel driver

(define (main job-file/files . commands)
  (let* ((source-files
	  (if (string? job-file/files)
	      (map symbol->string (file->list job-file/files))
	      job-file/files))
	 (full-source
	  (apply append (map file->list source-files)))
	 (def-function*
	   (filter (lambda (defn)  (equal? (car defn) 'define))
		   full-source))
	 (d* (begin
	       (set-abssyn-maybe-coerce! #f)
	       (scheme->abssyn-d def-function* '())))
	 (d* (anf-convert d*))
	 (flowmap #f))
    (let loop ((commands commands))
      (if (null? commands)
	  'done
	  (let ((command (car commands)))
	    (case command
	      ((alloc-flowmap)  (set! flowmap (new-flowmap)))
	      ((alloc-reachmap) (allocate-pass-return! *anf-app-map*))
	      ((alloc-single)   (allocate-avals-single-astore d* flowmap))
	      ((alloc-multiple) (allocate-avals-multiple-astore d* flowmap))
	      ((environment)    (environment-constraints-d* d* flowmap))
	      ((must-alias)     (must-alias-constraints-d* d* flowmap))
	      ((store)          (store-constraints-d* d* flowmap))
	      ((collect)	(collect! *anf-app-map* flowmap))
	      ((disp-def)       (anf-display-d* d*))
	      ((disp-flowmap)   (flowmap-display flowmap))
	      ((disp-varcardmap) (varcardmap-display))
	      ((disp-refcardmap) (refcardmap-display))
	      ((disp-reachmap)  (reachmap-display *anf-app-map*))
	      ((disp-astore)	(all-astore-display))
	      ((disp-init)	(all-init-display))
	      ((disp-ana-var)	(display-analyze-varcardmap))
	      ((disp-ana-ref)	(display-analyze-refcardmap))
	      ((latex-def)	(latex-display-anf-d* d*))
	      ((latex-flowmap)	(latex-display-flowmap flowmap))
	      ((latex-varcardmap) (latex-display-varcardmap))
	      ((latex-refcardmap) (latex-display-refcardmap))
	      ((latex-reachmap) (latex-display-reachmap *anf-app-map*))
	      ((latex-astore)	(latex-display-all-astore))
	      ((latex-init)	(latex-display-all-init))
	      (else
	       (if (pair? command)
		   (case (car command)
		     ((fix) (save-reachmap! *anf-app-map*)
			    (loop (cdr command))
			    (if (equal-reachmap? *anf-app-map*)
				'fixpoint-reached
				(loop commands)))
		     ((begin) (loop (cdr command))
			      (loop commands))))))
	    (loop (cdr commands))))))) 

