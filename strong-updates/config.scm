(define-interface a-normal-form-interface
  (export anf-lambda? anf-lambda->free anf-lambda->formals anf-lambda->body
	  anf-ref?
	  anf-let? anf-let->info anf-let->formal anf-let->info! anf-let->header anf-let->body
	  anf-unit->info! anf-unit? anf-unit->info anf-unit->body
	  anf-cond? anf-cond->info anf-cond->info! anf-cond->then anf-cond->else
	  anf-def->name anf-def->body
	  anf-app->rpass! anf-app->rreturn!
	  anf-def->last
	  anf-var? anf-var->name
	  anf-const?
	  anf-op?
	  anf-cond->last
	  anf-call?
	  anf-lambda->nr
	  anf-app? anf-app->rator anf-app->rands
	  anf-lambda->last
	  anf-ref->nr anf-ref->actual
	  anf-deref? anf-deref->actual
	  anf-assign? anf-assign->ref anf-assign->actual
	  anf-celleq?
	  anf-cond->last-then anf-cond->last-else
	  anf-app->rpass anf-app->rreturn
	  anf-get-last
	  anf-lambda->nr!
	  anf-ref->nr!
	  anf-app->nr!
	  anf-lambda->nr
	  anf-ref->nr
	  anf-app->nr
	  make-anf-def
	  make-anf-var
	  make-anf-let
	  make-anf-unit
	  make-anf-const
	  make-anf-cond
	  make-anf-op
	  make-anf-app
	  make-anf-lambda
	  make-anf-ctor
	  make-anf-sel
	  make-anf-test
	  make-anf-ref
	  make-anf-deref
	  make-anf-assign
	  make-anf-celleq
	  ))

(define-interface anf-converter-interface
  (export anf-convert anf-convert-d anf-convert-e* anf-convert-e anf-convert-top
	  *anf-pp-count* *anf-pp-map* *anf-app-map*))

(define-interface toplevel-interface
  (export main))

(define-interface finite-map-interface
  (export make-symbol-fm make-number-fm
	  fm-lookup fm-update! fm-walk))

(define-interface analysis-interface
  (export allocate-avals-single-astore
	  allocate-avals-multiple-astore
	  allocate-avals-d*
	  allocate-pass-return!
	  environment-constraints-d*
	  must-alias-constraints-d*
	  store-constraints-d*
	  collect!
	  new-flowmap
	  *anf-app-map*
	  save-reachmap!
	  equal-reachmap?
	  flowmap-display	latex-display-flowmap
	  anf-display-d*	latex-display-anf-d*
	  refcardmap-display	latex-display-refcardmap
	  varcardmap-display	latex-display-varcardmap
	  reachmap-display	latex-display-reachmap
	  all-astore-display	latex-display-all-astore
	  all-init-display	latex-display-all-init
	  display-analyze-varcardmap
	  display-analyze-refcardmap
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-structure a-normal-form a-normal-form-interface
  (open scheme signals
	cogen-record cogen-labset-bylist))

(define-structure anf-converter anf-converter-interface
  (open scheme signals
	auxiliary
	cogen-abssyn
	a-normal-form)
  (files anf-convert))

(define-structure toplevel toplevel-interface
  (open scheme signals auxiliary
	cogen-scheme cogen-globals analysis)
  (files toplevel))

(define-structure finite-map finite-map-interface
  (open scheme features
	auxiliary cogen-record)
  (files finite-map))

(define-structure analysis (compound-interface analysis-interface
					       anf-converter-interface)
  (open scheme signals
	cogen-record cogen-labset-bylist
	auxiliary cogen-abssyn		;for anf-convert
	;; a-normal-form anf-converter	;for *anf-app-map* *anf-pp-map* *anf-pp-count*
	finite-map)
  (files a-normal-form anf-convert analysis))
