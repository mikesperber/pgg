(define-interface cogen-cps-genext-interface
  (export _app
	  _app_memo
	  ((_lambda_memo _let _ctor_memo _st_memo _op _lift0 _lift start-memo)
	   :syntax)
	  _lambda
	  _begin
	  _If
	  _Eval
	  nextlevel
	  start-memo-internal
	  multi-memo
	  multi-memo-no-result
	  ))

(define-interface cogen-direct-syntax-interface
  (export _vlambda
	  nextlevel
	  start-memo-internal
	  multi-memo
	  multi-memo-no-result
	  ((_app
	    _app_memo
	    _lambda
	    _lambda_memo
	    _let
	    _begin
	    _ctor_memo
	    _s_t_memo
	    _if
	    _op
	    _lift0
	    _lift
	    _eval
	    start-memo)
	   :syntax)))

(define-interface cogen-direct-interface
  (export _app
	  _app_memo
	  _lambda
	  _lambda_memo
	  _let
	  _begin
	  _ctor_memo
	  _s_t_memo
	  _If
	  _If1
	  _Op
	  _Lift0
	  _Lift
	  _Eval
	  start-memo
	  multi-memo
	  multi-memo-no-result))

(define-interface cogen-direct-anf-interface
  (export ((_app
	    _app_memo
	    _lambda
	    _lambda_memo
	    _vlambda
	    _vlambda_memo
	    _begin
	    _ctor_memo
	    _s_t_memo
	    _if
	    _op
	    _op_pure
	    _freevar
	    _lift0
	    _lift
	    _eval
	    _MAKE-CELL_MEMO
	    _CELL-SET!_MEMO
	    _CELL-EQ?_MEMO)
	   :syntax)))

(define-interface cogen-anf-compile-interface
  (export _vlambda
	  start-memo-internal
	  multi-memo
	  multi-memo-no-result
	  ((_app
	    _app_memo
	    _lambda
	    _lambda_memo
	    _let
	    _begin
	    _ctor_memo
	    _s_t_memo
	    _if
	    _op
	    _op_serious
	    _lift0
	    _lift
	    _eval
	    _var
	    start-memo)
	   :syntax)))

(define-interface cogen-incremental-interface
  (export multi-memo
	  multi-memo-no-result))

(define-interface cogen-intro-interface
  (export ((_var
	    _app
	    _lambda
	    _lambda_memo
	    _begin
	    _ctor_memo
	    _sel_memo
	    _if
	    _op
	    _lift0
	    _lift
	    _eval
	    start-memo)
	   :syntax)
	  _let
	  nextlevel
	  multi-memo
	  multi-memo-no-result))

(define-interface cogen-memo-interface
  (export ((start-memo) :syntax)
	  specialize
	  nextlevel
	  multi-memo
	  multi-memo-no-result))

(define-interface cogen-record-interface
  (export ((define-record) :syntax)))

(define-interface cogen-residual-interface
  (export make-residual-apply
	  make-residual-let
	  make-residual-let-serious
	  make-residual-let-trivial
	  make-residual-begin
	  make-residual-cons
	  make-residual-define-data
	  make-residual-define-mutable
	  make-residual-if
	  make-residual-call
	  make-residual-closed-lambda
	  make-residual-literal
	  make-residual-generator
	  make-residual-definition!))

(define-interface shift-reset-interface
  (export ((shift reset) :syntax)
	  *shift *reset))

(define-interface define-data-interface
  (export ((define-data) :syntax)))

(define-interface cogen-library-interface
  (export static-constructor
	  top-project-static
	  top-project-dynamic
	  project-dynamic-level
	  top-clone-dynamic
	  top-clone-with
	  multi-append
	  static-cell
	  static-vector
	  current-static-store!
	  install-static-store!
	  initialize-static-store!
	  address-registry-reset!
	  address-map-reset!
	  creation-log-initialize!
	  creation-log-push!
	  creation-log-pop!
	  binding-times))

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
	  ))

(define-interface cogen-specialize-interface
  (export *memolist* *residual-program* *support-code*
	  add-to-memolist! clear-memolist! lookup-memolist
	  set-residual-program! add-to-residual-program! clear-residual-program!
	  add-to-support-code! clear-support-code!
	  gen-address-reset! gen-address
))

(define-interface auxiliary-interface
  (export id succ pred
	  any->symbol
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

(define-structure cogen-library cogen-library-interface
  (open scheme signals auxiliary cogen-specialize cogen-gensym cogen-boxops)
  (files cogen-library))

(define-structure shift-reset
  shift-reset-interface
  (open scheme signals escapes)
  (files shift-reset))

(define-structure cogen-residual
  cogen-residual-interface
  (open scheme cogen-specialize)
  (files cogen-residual))

(define-interface cogen-completers-interface
  (export ((_complete
	    _complete-no-result
	    _complete-serious
	    _complete-serious-no-result
	    _complete-maybe) :syntax)))

(define-interface cogen-record-interface
  (export ((define-record) :syntax)))

(define-structure cogen-record cogen-record-interface
  (open scheme signals)
  (files cogen-record))

(define-structure cogen-memo-standard
  cogen-memo-interface
  (open scheme auxiliary shift-reset
	cogen-specialize cogen-globals cogen-record cogen-gensym
	cogen-library cogen-completers cogen-residual)
  (files cogen-memo-standard))

(define-structure pgg-library
  (compound-interface cogen-residual-interface
		      cogen-direct-anf-interface
		      cogen-memo-interface
		      cogen-boxops-interface)
  (open scheme escapes signals auxiliary
	cogen-gensym cogen-boxops cogen-globals cogen-library
	shift-reset cogen-completers cogen-memo-standard cogen-residual )
  (files cogen-direct-anf))

(define-structure pgg-residual
  (export ((start-memo define-data) :syntax)
	  specialize
	  make-cell cell-ref cell-set!)
  (open scheme escapes pgg-library cogen-boxops define-data))

(define-structure cogen-completers
  cogen-completers-interface
  (open scheme shift-reset cogen-residual cogen-gensym)
  (files cogen-completer))

(define-structure pgg-specialize
  (export specialize
	  *residual-program*
	  *support-code*)
  (open cogen-memo-standard
	cogen-specialize))

