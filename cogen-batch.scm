;; cogen-batch.scm:
;; Run the cogen and generating extensions in batch mode
;; needs big-scheme, i/o, handle, conditions
;; pp, and fname.scm (or scsh, for that matter ...)

(define cogen-usage-text
'("Usage:"
  "Explain me:"
  "cps-mcogen ( --help | -h )"
  ""
  "just create a generating extension:"
  "cps-mcogen ( -c callpat | --call-pattern=callpat )"
  "           [ -o outfile ] [ --output=outfile ]"
  "           [ --keep-generator ]"
  "           [ --generator=genname ] [ -g genname ]"
  "           [ -v ] [ --verbose ]"
  "           subjectfile ..."
  ""
  "do everything:"
  "cps-mcogen ( -c callpat | --call-pattern=callpat )"
  "           ( -i infile | --input=infile )"
  "           [ -p goalproc ] [ --goal-proc=goalproc ]"
  "           [ -s supportfile+ ] [ --support=supportfile+ ]"
  "           [ -o outfile ] [ --output=outfile ]"
  "           [ -v ] [ --verbose ]"
  "           subjectfile ..."
  ""
  "specialize with pre-fabricated generating extension:"
  "cps-mcogen ( -i infile | --input=infile )"
  "           [ -p goalproc ] [ --goal-proc=goalproc ]"
  "           [ -s supportfile+ ] [ --support=supportfile+ ]"
  "           [ -o outfile ] [ --output=outfile ]"
  "           [ -v ] [ --verbose ]"
  "           genextfile ..."))

(define (cogen-display-usage)
  (for-each (lambda (line)
	      (display line) (newline))
	    cogen-usage-text))

(define cogen-options
  '(("help" "h" help)
    ("v" "verbose" verbose)
    ("c" "call-pattern" parameter call-pattern)
    ("i" "input" parameter accumulates splits input)
    ("o" "output" parameter output)
    ("p" "goal-proc" parameter goal-proc)
    ("s" "support" parameter accumulates splits support)
    ("keep-generator" keep-generator)
    ("generator" parameter generator)))

(define scheme-file-suffixes '(".scm" ".ss"))
(define input-file-suffixes '(".dat"))
(define cogen-generating-extension-file-suffix "-genext")
(define cogen-generator-file-suffix "-generator")

(define cogen-memo-template-prefix ";; memo-template: ")

(define (canonical-generator-name name)
  (call-with-values
   (lambda ()
     (parse-file-name name))
   (lambda (dir base suffix)
     (string-append dir base cogen-generator-file-suffix suffix))))

(define (canonical-genext-name name)
  (call-with-values
   (lambda ()
     (parse-file-name name))
   (lambda (dir base suffix)
     (string-append dir base cogen-generating-extension-file-suffix suffix))))

(define (maybe-sans-suffix string suffix)
  (let ((length (string-length string))
	(suffix-length (string-length suffix)))
    (if (and (>= length suffix-length)
	     (string=? (substring string
				  (- length suffix-length) length)
		       suffix))
	(substring string 0 (- length suffix-length))
	string)))

(define (canonical-residual-name name inputs)
  (call-with-values
   (lambda ()
     (parse-file-name name))
   (lambda (dir base suffix)
     (let ((base (maybe-sans-suffix 
		  base cogen-generating-extension-file-suffix)))
       (string-append
	dir base
	(apply string-append
	       (map (lambda (input)
		      (string-append
		       "-"
		       (file-name-sans-extension
			(file-name-nondirectory input))))
		    inputs))
	suffix)))))

(define (read-line . arg)
  (let* ((port (if (null? arg)
		   (current-input-port)
		   (car arg)))
	 (char (read-char port)))
    (if (eof-object? char)
	char
	(do ((char char (read-char port))
	     (clist '() (cons char clist)))
	    ((or (eof-object? char) (char=? #\newline char))
	     (list->string (reverse clist)))))))

(define (read-memo-template)
  (let* ((line (read-line))
	 (line-length (string-length line))
	 (prefix-length (string-length cogen-memo-template-prefix)))

    (if (or (< line-length prefix-length)
	    (not (string=? cogen-memo-template-prefix
			   (substring line 0 prefix-length))))
	(error "invalid memo template: ~a" line))

    (let* ((memo-stuff (substring line prefix-length line-length))
	   (port (make-string-input-port memo-stuff)))
      (read port))))

(define (pp-list l . args)
  (for-each (lambda (x)
	      (apply pp x args))
	    l))

(define (cogen-make-generator files pattern)
  (cogen-driver files pattern))

(define (cogen-make-generating-extension pattern)
  (if (> (length pattern) 16)
      (error "Too many parameters for the goal procedure~%"))
  
  ;; let a guy have some fun ...
  (values
   (start-memo-internal
    2
    '$goal
    (eval '$goal (interaction-environment))
    (map (lambda (level) (+ 1 level)) pattern)
    (take (length pattern)
	  '(foo bar baz bla baz blabla argl blabaz foobar
	    kotz brech wuerg reier goebel kuebel spei)))
   *residual-program*
   *support-code*))

(define (cogen-run-generating-extension memo-template input-parameters)
  (let ((next-memo-template
	 (nextlevel memo-template input-parameters)))
    (values
     next-memo-template
     *residual-program*
     *support-code*)))
  
(define (cogen-main argv)
  (call-with-current-continuation
   (lambda (real-exit)

     (define (exit code)
       (force-output (current-output-port))
       (force-output (current-error-port))
       (real-exit code))

     (with-handler

      (lambda (condition decline)
	(decline)
	(if (error? condition)
	    (apply format (current-error-port) (condition-stuff condition)))
	(exit 1))

      (lambda ()
	(call-with-values
	 (lambda ()
	   (with-handler
	    ;; we expect only errors here ...
	    (lambda (condition decline)
	      (apply format (current-error-port) (condition-stuff condition))
	      (cogen-display-usage)
	      (exit 1))
	    (lambda ()
	      (get-options cogen-options argv))))

	 (lambda (options files)

	   ;; mode 0
	   (if (assq 'help options)
	       (begin
		 (cogen-display-usage)
		 (exit 0)))
	   
	   (if (null? files)
	       (error "No input files specified.~%"))

	   (let ((verbose? (assq 'verbose options))
		 (the-genext #f)
		 (the-memo-template #f))
	  
	     (define (maybe-format . l)
	       (if verbose?
		   (begin 
		     (apply format #t l)
		     (force-output (current-output-port)))))

	     ;; mode 1 or 2---make a generating extension
	     (if (assq 'call-pattern options)
		 (begin
		   (maybe-format "Creating a generating extension~%")
		   (let* ((pattern-string
			   (cdr (assq 'call-pattern options)))
			  (pattern
			   (read (make-string-input-port pattern-string)))
			  (generator
			   (begin
			     (maybe-format "Making the generator~%")
			     (cogen-make-generator files pattern))))

		     (if (or (assq 'keep-generator options)
			     (assq 'generator options))
			 (let ((generator-name
				(let ((p (assq 'generator options)))
				  (if p
				      (cdr p)
				      (canonical-generator-name (car files))))))
			   (begin
			     (maybe-format "Writing generator into ~A~%"
					   generator-name)
			     (with-output-to-file
				 generator-name
			       (lambda ()
				 (pp-list generator))))))

		     (maybe-format "Loading the generator~%")
		     (load-program generator)
			   
		     (call-with-values
		      (lambda ()
			(maybe-format "Making the generating extension~%")
			(cogen-make-generating-extension (cdr pattern)))
		      (lambda (memo-template genext support-code)
			(if (assq 'input options)
			    ;; specialize also---later
			    (begin
			      (set! the-genext (append genext support-code))
			      (set! the-memo-template memo-template))
			    ;; just write out the generating extension
			    (let ((genext-name
				   (if (assq 'output options)
				       (cdr (assq 'output options))
				       (canonical-genext-name (car files)))))
			      (maybe-format "Writing generating extension into ~A~%"
					    genext-name)
			      (with-output-to-file
				  genext-name
				(lambda ()
				  (format #t "~a~s~%~%"
					  cogen-memo-template-prefix
					  memo-template)
				  (pp-list genext)
				  (if (not (null? support-code))
				      (format #t "~%;; Supporting definitions:~%"))
				  (pp-list support-code))))))))))

	     ;; mode 2 or 3---specialize something
	     (if (assq 'input options)
		 (let ((input-files (cdr (assq 'input options))))

		   (maybe-format "Running a generating extension~%")

		   (if (assq 'support options)
		       (begin
			 (maybe-format "Loading support code~%")
			 (for-each (lambda (support-file)
				     (maybe-format "Loading ~A~%" support-file)
				     (load support-file))
				   (cdr (assq 'support options)))))

		   (maybe-format "Loading generating extension ")
		   (if (assq 'call-pattern options)
		       ;; we have one in memory
		       (begin
			 (maybe-format "from memory~%")
			 (load-program the-genext))
		       (begin
			 (maybe-format "from files~%")
			 (set! the-memo-template
			       (with-input-from-file
				   (car files)
				 read-memo-template))
			 (for-each (lambda (file)
				     (maybe-format "Loading ~A~%" file)
				     (load file))
				   files)))

		   (let ((input-parameters
			  (begin
			    (maybe-format "Parsing input from files~%")
			    (let loop ((input-files input-files))
			      (if (null? input-files)
				  '()
				  (begin
				    (maybe-format "Parsing ~A~%" (car input-files))
				    (append (file->list (car input-files))
					    (loop (cdr input-files))))))))
			 (goal (if (assq 'goal-proc options)
				   (cdr (assq 'goal-proc options))
				   '$goal))
			 (residual-program-name
			  (if (assq 'output options)
			      (cdr (assq 'output options))
			      (canonical-residual-name (car files) input-files))))
		     
		     (maybe-format "Specializing with goal ~A~%" goal)
		     (call-with-values
		      (lambda ()
			(cogen-run-generating-extension 
			 the-memo-template input-parameters))
		      (lambda (memo-template code support-code)
			(maybe-format "Writing residual program into ~A~%"
				      residual-program-name)
			(with-output-to-file
			    residual-program-name
			  (lambda ()
			    (format #t "~a~s~%~%"
				    cogen-memo-template-prefix
				    memo-template)
			    (pp-list code)
			    (if (not (null? support-code))
				(format #t "~%;; Supporting definitions:~%"))
			    (pp-list support-code))))))))

	     (exit 0)))))))))
