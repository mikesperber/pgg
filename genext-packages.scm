;;; genext-packages.sc

;;; copyright © 1996-2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

(define-interface cogen-direct-anf-interface
  (export ((_app
	    _app_memo
	    _lambda
	    _lambda_memo
	    _lambda_poly
	    _vlambda
	    _vlambda_memo
	    _begin
	    _ctor_memo
	    _s_t_memo
	    _if
	    _op
	    _op-serious
	    _op_pure
	    _freevar
	    _lift0
	    _lift
	    _eval
	    _run
	    _MAKE-CELL_MEMO
	    _CELL-EQ?_MEMO
	    _MAKE-VECTOR_MEMO
	    _MESSAGE!_MEMO)
	   :syntax)))

(define-interface cogen-incremental-interface
  (export multi-memo
	  multi-memo-no-result))

(define-interface cogen-memo-interface
  (export ((start-memo) :syntax)
	  specialize
	  prepare!
	  specialize-after-prepare
	  continue
	  nextlevel
	  suspend
	  resurrect
	  multi-memo
	  multi-memo-no-result))

(define-interface cogen-record-interface
  (export ((define-record) :syntax)))

(define-interface cogen-residual-interface
  (export make-residual-apply
	  make-residual-let
	  make-residual-let-serious
	  make-residual-let-serious-apply
	  make-residual-let-trivial
	  make-residual-begin
	  make-residual-cons
	  make-residual-define-data
	  make-residual-define-mutable
	  make-residual-if
	  make-residual-call
	  make-residual-primop
	  make-residual-closed-lambda
	  make-residual-literal
	  make-residual-variable
	  make-residual-generator-ve*
	  make-residual-generator-vve*
	  make-residual-generator-vve
	  make-residual-generator-vvve*
	  make-residual-generator-veve*
	  make-residual-generator-vvvve
	  make-residual-generator-vvee
	  make-residual-generator-vqqeqe
	  make-residual-generator-vqqqeqe
	  make-residual-definition!))

(define-interface shift-reset-interface
  (export ((shift reset) :syntax)
	  *shift *reset))

(define-interface define-data-interface
  (export ((define-data) :syntax)))

(define-interface cogen-library-interface
  (export static-constructor
	  hidden-constructor
	  poly-constructor
	  poly-registry-reset!
	  top-project-static
	  top-project-dynamic
	  top-clone-dynamic
	  top-clone-with
	  serialize
	  static-cell static-cell-at
	  static-vector
	  current-static-store!
	  install-static-store!
	  initialize-static-store!
	  address-registry-reset!
	  address-map-reset!
	  creation-log-initialize!
	  creation-log-push!
	  creation-log-pop!))

(define-interface cogen-globals-interface
  (export *bta-display-level*
	  *effect-display-level*
	  *scheme->abssyn-label-counter*
	  *scheme->abssyn-static-references*
	  *scheme->abssyn-let-insertion*
	  *abssyn-maybe-coerce*
	  *memo-optimize*
	  *memolist-stages*
	  *generating-extension*
	  *termination-analysis*
	  *generate-flat-program*
	  *lambda-is-pure*
	  *lambda-is-toplevel*
	  set-generate-flat-program!
	  set-bta-display-level!
	  set-effect-display-level!
	  set-scheme->abssyn-static-references!
	  set-scheme->abssyn-label-counter!
	  set-scheme->abssyn-let-insertion!
	  set-memo-optimize!
	  set-memolist-stages!
	  set-generating-extension!
	  set-abssyn-maybe-coerce!
	  set-termination-analysis!
	  set-lambda-is-pure!
	  set-lambda-is-toplevel!
	  ))

(define-interface cogen-specialize-interface
  (export *memolist* *residual-program* *support-code*
	  add-to-memolist! clear-memolist! lookup-memolist for-each-memolist
	  set-residual-program! add-to-residual-program! clear-residual-program!
	  add-to-support-code! clear-support-code!
	  add-to-deferred-list! clear-deferred-list!
	  lookup-deferred-list for-each-deferred-list
	  gen-address-reset! gen-address
))

(define-interface auxiliary-interface
  (export id succ pred
	  any->symbol trim-symbol
	  nlist
	  set-include set-union set-intersection set-subtract set-difference set-union* set-equal?
	  and-map and-map2 strict-and-map
	  or-map strict-or-map thread-map
	  generic-sort
	  filter
	  remove-duplicates
	  countq
	  list-or
	  take
	  ((load-program) :syntax)
	  file->list writelpp writel count-cells
	  display-line display-return display-list spaces
	  strip-path-prefix
	  strip-path-suffix))

(define-interface cogen-gensym-interface
  (export gensym-reset!
	  gensym
	  gensym-trimmed
	  gensym-local-reset!
	  gensym-local-push!
	  gensym-local-pop!
	  gensym-local-hold
	  gensym-local-push-old!
	  gensym-local
	  gensym-local-trimmed
	  gensym-ignore-name-stubs!
	  gensym-use-name-stubs!))

(define-structure cogen-gensym cogen-gensym-interface
  (open scheme auxiliary)
  (files cogen-gensym))

(define-interface cogen-boxops-interface
  (export make-cell cell-ref cell-set!))

(define-structure auxiliary auxiliary-interface
  (open scheme pp)
  (files auxiliary))

(define-structure cogen-specialize cogen-specialize-interface
  (open scheme cogen-globals)
  (files cogen-specialize))

(define-structure define-data define-data-interface
  (open scheme escapes)
  (files cogen-ctors))

(define-structure cogen-boxops cogen-boxops-interface
  (open scheme)
  (files cogen-boxops))

(define-structure cogen-globals cogen-globals-interface
  (open scheme)
  (files cogen-globals))

(define-structure shift-reset
  shift-reset-interface
  (open scheme signals escapes)
  (files shift-reset))

(define-structure cogen-residual cogen-residual-interface
  (open scheme cogen-gensym cogen-specialize)
  (files cogen-residual))

(define-interface cogen-completers-interface
  (export ((_complete
	    _complete-no-result
	    _complete-serious
	    _complete-serious-apply
	    _complete-serious-no-result
	    _complete-serious-apply-no-result
	    _complete-maybe) :syntax)))

(define-module (make-cogen-completers residual)
  (structure cogen-completers-interface
    (open scheme shift-reset residual cogen-gensym)
    (files cogen-completer)))

(def cogen-completers (make-cogen-completers cogen-residual))

(define-module (make-cogen-library residual completers)
  (structure cogen-library-interface
    (open scheme signals auxiliary shift-reset threads
	  completers cogen-specialize
	  cogen-gensym residual
	  cogen-boxops)
    (files cogen-library)))

(def cogen-library (make-cogen-library cogen-residual 
				       cogen-completers))

(define-interface cogen-record-interface
  (export ((define-record) :syntax)))

(define-structure cogen-record cogen-record-interface
  (open scheme signals)
  (files cogen-record))

(define-module (make-cogen-memo-standard library completers residual)
  (structure cogen-memo-interface
    (open scheme auxiliary signals shift-reset threads
	  cogen-specialize cogen-globals cogen-record cogen-gensym
	  library completers residual)
    (files cogen-memo-standard)))

(def cogen-memo-standard (make-cogen-memo-standard cogen-library
						   cogen-completers
						   cogen-residual))

(define-module (make-pgg-library residual library completers memo)
  (structure cogen-direct-anf-interface
    (open scheme escapes signals auxiliary threads placeholders
	  cogen-gensym cogen-boxops cogen-globals library
	  shift-reset completers memo residual)
    (files cogen-direct-anf)))

(def pgg-library (make-pgg-library cogen-residual
				   cogen-library
				   cogen-completers
				   cogen-memo-standard))

(define-module (make-pgg-residual memo)
  (structure (export ((start-memo define-data) :syntax)
		     specialize
		     make-cell cell-ref cell-set!)
    (open scheme escapes
	  define-data
	  cogen-boxops memo)))

(def pgg-residual (make-pgg-residual cogen-memo-standard))

(define-module (make-pgg-specialize memo)
  (structure (export specialize
		     *residual-program*
		     *support-code*)
    (open memo
	  cogen-specialize)))

(def pgg-specialize (make-pgg-specialize cogen-memo-standard))

