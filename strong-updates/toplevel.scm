;;; toplevel driver

(define (main job-file/files . commands)
  (let* ((source-files
	  (if (string? job-file/files)
	      (map (lambda (s) (if (symbol? s) (symbol->string s) s))
		   (file->list job-file/files))
	      job-file/files))
	 (full-source
	  (apply append (map file->list source-files)))
	 (def-function*
	   (filter (lambda (defn)  (equal? (car defn) 'define))
		   full-source))
	 (def-syntax*
	   (filter (lambda (defn) (eq? (car defn) 'define-syntax))
		   full-source))
	 (abssyn-d* (begin
		      (set-abssyn-maybe-coerce! #f)
		      (scheme->abssyn-d def-function*
					def-syntax*
					symtab-pairs)))
	 (d* (anf-convert abssyn-d*))
	 (freevars (anf-collect-freevars-d* d*))
	 (flowmap #f)
	 (file-name-prefix ""))
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
	      ((disp-freevars)  (anf-display-freevars-d* d*))
	      ((disp-flowmap)   (flowmap-display flowmap))
	      ((disp-varcardmap) (varcardmap-display))
	      ((disp-refcardmap) (refcardmap-display))
	      ((disp-reachmap)  (reachmap-display *anf-app-map*))
	      ((disp-astore)	(all-astore-display))
	      ((disp-init)	(all-init-display))
	      ((disp-ana-var)	(display-analyze-varcardmap freevars))
	      ((disp-ana-ref)	(display-analyze-refcardmap))
	      ((disp-var-stat)  (display-free-variable-statistics))
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
		     ((fix) (let fix-loop ()
			      (save-reachmap! *anf-app-map*)
			      (loop (cdr command))
			      (if (equal-reachmap? *anf-app-map*)
				  'fixpoint-reached
				  (fix-loop))))
		     ((file-name-prefix) (if (and (pair? (cdr command))
						  (string? (cadr command)))
					     (set! file-name-prefix (cadr command))))
		     ((begin) (if (and (pair? (cdr command))
				       (string? (cadr command)))
				  (with-output-to-file
				      (string-append file-name-prefix (cadr command))
				    (lambda ()
				      (loop (cddr command))))
				  (loop (cdr command))))
		     ((display) (display (cdr command))
				(newline))))))
	    (loop (cdr commands)))))))

(define symtab-pairs
  `((cons , (lambda (ctor args) (annMakeCtor ctor 0 '*dummy* args)) 2)
    (car  ,(annMakeSel1 'cons '*dummy* 1) 1)
    (cdr  ,(annMakeSel1 'cons '*dummy* 2) 1)))
