(if ***dynamic-context-propagation***
    (eval '(define-syntax maybe-reset
	     (syntax-rules () ((_ x) x)))
	  (interaction-environment))
    (eval '(define-syntax maybe-reset
	     (syntax-rules () ((_ x) (reset x))))
	  (interaction-environment)))



