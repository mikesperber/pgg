(define-structure small-big-scheme (export concatenate-symbol
					   error breakpoint
					   atom? null-list? neq? n=
					   identity no-op
					   memq? first any? any every? 
					   filter filter! filter-map partition-list partition-list!
					   remove-duplicates delq delq! delete
					   reverse!
					   (destructure :syntax)
					   (receive :syntax)
					   format
					   sort-list sort-list!)
  (open big-scheme))

(define-structure smurf-queues (compound-interface
				(interface-of queues)
				(export queue-assoc
					queue-any
					dequeue-first!))
  (open scheme-level-1 define-record-types signals small-big-scheme)
  (files ((=scheme48 big) queue)
	 smurf-queue)
  (optimize auto-integrate))

(define-structure broken-distributed-auxiliary auxiliary-interface
  (open auxiliary scheme)
  (files dummy-gensym))

(define-structure cogen-distributed-library cogen-library-interface
  (open scheme signals broken-distributed-auxiliary cogen-boxops)
  (files cogen-library))

(define-structure cogen-distributed-completers
  cogen-completers-interface
  (open scheme shift-reset cogen-residual broken-distributed-auxiliary)
  (files cogen-completer))

(define-structure cogen-memo-distributed
  (compound-interface cogen-memo-interface
		      (export multi-memo
			      multi-memo-no-result
			      start-specialization
			      collect-residual-program
			      display-kill-counts
			      display-elapsed-times))
  (open scheme shift-reset broken-distributed-auxiliary
	bitwise small-big-scheme smurf-queues tables
	cogen-distributed-library
	cogen-record cogen-distributed-completers cogen-residual
	cogen-wrapping
	time
	message-low aspaces proxies
	threads threads-internal locks placeholders)
  (files cogen-distributed-utils
	 cogen-spec-server
	 cogen-memo-master))
 
(define-structure pgg-distributed-library
  (compound-interface cogen-construct-genext-interface
		      cogen-residual-interface
		      cogen-direct-anf-interface
		      cogen-memo-interface)
  (open scheme escapes signals broken-distributed-auxiliary
	cogen-boxops cogen-globals cogen-distributed-library
	shift-reset cogen-distributed-completers cogen-memo-distributed cogen-residual)
  (files cogen-direct-anf))

(define-interface cogen-wrapping-interface
  (export wrap-program-point
	  unwrap-program-point
	  wrap-similar-program-point))

(define-structure cogen-wrapping cogen-wrapping-interface
  (open scheme aspaces closures)
  (files cogen-wrapping))

