
(define-interface cogen-abssyn-interface
  (export annMakeDef
	  annMakeDefWithoutMemoization
	  annDefFetchProcName
	  annDefFetchProcFormals
	  annDefFetchProcBody
	  annDefSetProcBody
	  annDefSetProcBTVar!
	  annDefFetchProcBTVar
	  annDefFetchProcEVar
	  annDefSetProcEVar!
	  annDefFetchProcAutoMemo
	  annDefSetProcAutoMemo!
	  annDefLookup
	  annExprFetchType
	  annExprSetType!
	  annExprFetchMemo
	  annExprSetMemo!
	  annExprFetchEffect
	  annExprSetEffect!
	  annExprFetchLevel
	  annExprSetLevel!
	  annMakeVar
	  annIsVar?
	  annFetchVar
	  annMakeConst
	  annIsConst?
	  annFetchConst
	  annMakeCond
	  annIsCond?
	  annFetchCondTest
	  annFetchCondThen
	  annFetchCondElse
	  INTERNAL-IDENTITY
	  annMakeOp
	  annMakeOpaqueOp
	  annMakeFullOp
	  annMakeOp1
	  annIsOp?
	  annFetchOpName
	  annFetchOpArgs
	  annFetchOpDiscardability
	  annFetchOpProperty
	  annFetchOpPostprocessor
	  annFetchOpType
	  annMakeCall
	  annIsCall?
	  annFetchCallName
	  annFetchCallArgs
	  annMakeLet
	  annIsLet?
	  annFetchLetVar
	  annFetchLetHeader
	  annFetchLetBody
	  annFetchLetUnfoldability
	  annSetLetUnfoldability!
	  annFetchLetUseCount
	  annSetLetUseCount!
	  annMakeVLambda
	  annIsVLambda?
	  annFetchVLambdaFixedVars
	  annFetchVLambdaVar
	  annFetchVLambdaBody
	  annSetVLambdaBody!
	  annFetchVLambdaLabel
	  annFetchVLambdaBTVars
	  annSetVLambdaBTVars!
	  annMakeLambda
	  annIsLambda?
	  annFetchLambdaVars
	  annFetchLambdaBody
	  annSetLambdaBody!
	  annFetchLambdaLabel
	  annFetchLambdaBTVars
	  annSetLambdaBTVars!
	  annMakeApp
	  annIsApp?
	  annFetchAppRator
	  annFetchAppRands
	  annMakeCtor
	  annIsCtor?
	  annFetchCtorName
	  annFetchCtorDesc
	  annFetchCtorArgs
	  annFetchCtorLabel
	  annMakeSel
	  annMakeSel1
	  annIsSel?
	  annFetchSelName
	  annFetchSelDesc
	  annFetchSelComp
	  annFetchSelArg
	  annMakeTest
	  annMakeTest1
	  annIsTest?
	  annFetchTestName
	  annFetchTestDesc
	  annFetchTestArg
	  annMakeRef
	  annIsRef?
	  annFetchRefArg
	  annFetchRefLabel
	  annMakeDeref
	  annIsDeref?
	  annFetchDerefArg
	  annMakeAssign
	  annIsAssign?
	  annFetchAssignRef
	  annFetchAssignArg
	  annFetchAssignLabel
	  annMakeCellEq
	  annIsCellEq?
	  annFetchCellEqArgs
	  annMakeEval
	  annIsEval?
	  annFetchEvalBody
	  annFetchEvalDiff
	  annSetEvalDiff!
	  annFetchEvalQuoted
	  annSetEvalQuoted!
	  annIntroduceLift
	  annMakeLift
	  annIsLift?
	  annSetLiftDiff!
	  annFetchLiftDiff
	  annFetchLiftBody
	  annIntroduceMemo
	  annIntroduceMemo1
	  annMakeMemo
	  annIsMemo?
	  annSetMemoVars!
	  annFetchMemoVars
	  annFetchMemoBody
	  annFetchMemoLevel
	  ann-maybe-coerce
	  annFreeVars
	  annExprTerminates?))

(define-interface cogen-construct-genext-interface
  (export make-ge-var
	  make-ge-const
	  make-ge-cond
	  make-ge-op
	  make-ge-call
	  make-ge-let
	  make-ge-begin
	  make-ge-lambda-memo
	  make-ge-vlambda-memo
	  make-ge-app-memo
	  make-ge-lambda
	  make-ge-app
	  make-ge-ctor-memo
	  make-ge-sel-memo
	  make-ge-test-memo
	  make-ge-ctor
	  make-ge-sel
	  make-ge-test
	  make-ge-lift
	  make-ge-eval
	  make-ge-make-cell-memo
	  make-ge-cell-ref-memo
	  make-ge-cell-set!-memo
	  make-ge-cell-eq?-memo))

(define-interface shift-reset-interface
  (export ((shift reset) :syntax)))

(define-structure shift-reset
  shift-reset-interface
  (open scheme signals escapes)
  (files shift-reset))

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
	  ))

(define-interface define-data-interface
  (export ((define-data) :syntax)))

(define-structure define-data define-data-interface
  (open scheme escapes)
  (files cogen-ctors))

(define-interface cogen-direct-syntax-interface
  (export _vlambda
	  nextlevel
	  start-memo-internal
	  multi-memo
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
	  multi-memo))

(define-interface cogen-direct-anf-interface
  (export _vlambda
	  ((_app
	    _app_memo
	    _lambda
	    _lambda_memo
	    _begin
	    _ctor_memo
	    _s_t_memo
	    _if
	    _op
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

(define-interface cogen-directives-interface
  (export ((define-without-memoization
	     define-memo
	     define-primitive)
	   :syntax)))

(define-structure cogen-directives cogen-directives-interface
  (open scheme)
  (files cogen-directives))

(define-interface cogen-drive-interface
  (export cogen-driver))

(define-interface cogen-env-interface
  (export the-empty-env
	  extend-env
	  extend-env*
	  apply-env))

(define-structure cogen-env cogen-env-interface
  (open scheme signals cogen-record)
  (files cogen-env))

(define-interface cogen-bta-interface
  (export 
	  bta-run
	  node-fetch-type
	  ann->bt
	  full-ecr
	  wft-make-memo-property
	  wft-property-table
	  bta-make-memo-postprocessor
	  type->memo
	  ctor-function ctor-reference ctor-top))

(define-structure cogen-bta cogen-bta-interface
  (open scheme signals auxiliary
	cogen-globals pretty-print cogen-env
	cogen-typesig cogen-abssyn cogen-abssyn cogen-record cogen-labset)
  (files cogen-eq-flow
	 cogen-effect))

(define-interface cogen-effect-interface
  (export *effect-display-level*
	  effect-analysis
	  effect-label->type
	  effect-for-each
	  effect->labset))

(define-interface cogen-incremental-interface
  (export multi-memo))

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
	  multi-memo))

(define-interface cogen-library-interface
  (export static-constructor
	  top-project-static
	  top-project-dynamic
	  project-dynamic-level
	  top-clone-dynamic
	  top-clone-with
	  multi-append
	  static-cell
	  current-static-store!
	  install-static-store!
	  address-registry-reset!
	  address-map-reset!
	  creation-log-initialize!
	  creation-log-push!
	  creation-log-pop!
	  binding-times))

(define-structure cogen-library cogen-library-interface
  (open scheme signals auxiliary cogen-boxops)
  (files cogen-library))

(define-interface cogen-boxops-interface
  (export make-cell cell-ref cell-set!))

(define-structure cogen-boxops cogen-boxops-interface
  (open scheme)
  (files cogen-boxops))

(define-interface cogen-oca-interface
  (export oca-d))

(define-structure cogen-oca cogen-oca-interface
  (open scheme signals
	auxiliary
	cogen-abssyn)
  (files cogen-oca))

(define-interface cogen-record-interface
  (export ((define-record) :syntax)))

(define-structure cogen-record cogen-record-interface
  (open scheme signals)
  (files cogen-record))

(define-interface cogen-residual-interface
  (export make-residual-apply
	  make-residual-let
	  make-residual-let-serious
	  make-residual-let-trivial
	  make-residual-begin
	  make-residual-cons
	  make-residual-define-data
	  make-residual-if
	  make-residual-call
	  make-residual-closed-lambda
	  make-residual-literal
	  make-residual-definition!))

(define-structure cogen-residual
  cogen-residual-interface
  (open scheme auxiliary)
  (files cogen-residual))

(define-interface cogen-typesig-interface
  (export parse-type desc-type desc-ctor desc-np desc-nc desc-nt))

(define-structure cogen-typesig cogen-typesig-interface
  (open scheme cogen-record cogen-env)
  (files cogen-typesig))

(define-interface cogen-scheme-interface
  (export scheme->abssyn-define-type
	  scheme->abssyn-d
	  desc-type desc-np desc-nc desc-nt))

(define-structure cogen-scheme cogen-scheme-interface
  (open scheme auxiliary
	cogen-globals cogen-typesig
	cogen-abssyn cogen-env cogen-record cogen-bta)
  (files cogen-scheme))

(define-interface cogen-globals-interface
  (export *bta-display-level*
	  *effect-display-level*
	  *scheme->abssyn-label-counter*
	  *scheme->abssyn-static-references*
	  *scheme->abssyn-let-insertion*
	  *abssyn-maybe-coerce*
	  *memo-optimize*
	  *generating-extension*
	  set-bta-display-level!
	  set-effect-display-level!
	  set-scheme->abssyn-static-references!
	  set-scheme->abssyn-label-counter!
	  set-scheme->abssyn-let-insertion!
	  set-memo-optimize!
	  set-generating-extension!
	  set-abssyn-maybe-coerce!
	  ))

(define-structure cogen-globals cogen-globals-interface
  (open scheme)
  (files cogen-globals))

(define-interface cogen-skeleton-interface
  (export generate-d
	  *generating-extension*))

(define-structure cogen-skeleton cogen-skeleton-interface
  (open scheme auxiliary signals
	cogen-globals
	cogen-abssyn
	cogen-oca
	cogen-bta
	pgg-library)
  (files cogen-skeleton))

(define-interface cogen-labset-interface
  (export empty-labset
	  labset-empty?
	  labset-singleton
	  labset-intersection
	  labset-remove
	  labset-add
	  labset-union
	  labset-union*
	  labset-subtract
	  labset-subset?
	  labset-elem?
	  labset-equal?
	  labset-for-each
	  labset->list
	  set-labset-size!))

(define-interface pgg-interface
  (export cogen-driver))

(define-interface pretty-print-interface
  (export p))

(define-interface auxiliary-interface
  (export id succ pred
	  gensym gensym-reset!
	  gensym-local gensym-local-reset! gensym-local-push!
	  gensym-local-pop!
	  any->symbol
	  gen-address-reset! gen-address
	  *memolist* *residual-program* *support-code*
	  add-to-memolist! clear-memolist! lookup-memolist
	  set-residual-program! add-to-residual-program! clear-residual-program!
	  add-to-support-code! clear-support-code!
	  nlist
	  set-union set-subtract set-difference set-union* set-equal?
	  and-map and-map2 strict-and-map
	  or-map strict-or-map thread-map
	  generic-sort
	  filter
	  remove-duplicates
	  any?
	  take
	  ((load-program) :syntax)
	  file->list writelpp writel))

(define-structure auxiliary auxiliary-interface
  (open scheme pretty-print)
  (files auxiliary))

(define-structure pretty-print pretty-print-interface
  (open scheme)
  (files pp)
  (begin (define p pretty-print)))

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
					queue-any))
  (open scheme-level-1 define-record-types signals small-big-scheme)
  (files ((=scheme48 big) queue)
	 smurf-queue)
  (optimize auto-integrate))

(define-structure cogen-abssyn cogen-abssyn-interface
  (open scheme signals auxiliary cogen-globals)
  (files cogen-abssyn))

(define-structure cogen-labset cogen-labset-interface
  (open scheme signals)
  (files cogen-labset-bylist))

(define-structure pgg pgg-interface
  (open scheme auxiliary
	cogen-scheme
	cogen-bta
	cogen-skeleton)
  (files cogen-driver))

(define-structure pgg-library
  (compound-interface cogen-construct-genext-interface
		      cogen-residual-interface
		      cogen-direct-anf-interface
		      cogen-memo-interface)
  (open scheme escapes signals auxiliary
	cogen-boxops cogen-globals cogen-library
	shift-reset cogen-completers cogen-memo-standard cogen-residual )
  (files cogen-direct-anf))

(define-structure pgg-residual
  (export ((start-memo define-data) :syntax)
	  make-cell cell-ref cell-set!)
  (open scheme escapes pgg-library cogen-boxops)
  (files cogen-ctors))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-interface cogen-completers-interface
  (export ((_complete _complete-serious) :syntax)))

(define-structure cogen-completers
  cogen-completers-interface
  (open scheme shift-reset cogen-residual auxiliary)
  (files cogen-completer))

(define-interface cogen-memo-interface
  (export ((start-memo) :syntax)
	  nextlevel
	  multi-memo))

(define-structure cogen-memo-standard
  cogen-memo-interface
  (open scheme auxiliary shift-reset cogen-record
	cogen-library cogen-completers cogen-residual)
  (files cogen-memo-standard))

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
			      start-specialization
			      collect-residual-program
			      display-kill-counts))
  (open scheme shift-reset broken-distributed-auxiliary
	bitwise small-big-scheme smurf-queues
	cogen-distributed-library
	cogen-record cogen-distributed-completers cogen-residual
	cogen-wrapping
	message-low aspaces proxies threads threads-internal locks placeholders)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-structure reaching-definitions pgg-interface
  (open scheme signals auxiliary pretty-print
	cogen-abssyn cogen-env cogen-globals cogen-labset cogen-record
	cogen-typesig 
	cogen-scheme)
  (files cogen-reach))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-structure pgg-compiler-library
  (compound-interface cogen-construct-genext-interface
		      anf-specializer-interface
		      cogen-anf-compile-interface)
  (open scheme escapes signals auxiliary
	cogen-globals cogen-library cogen-boxops
	anf-specializer)
  (files
	 cogen-ctors
	 shift-reset
	 cogen-anf-compile
	 ))

(define-structure pgg-compiler
  (export cogen-driver)
  (open scheme auxiliary
	cogen-scheme cogen-bta
	cogen-compile-skeleton
	)
  (files cogen-driver))

;;; this is really a case for a higher-order module,
;;; parameterized over the pgg-compiler-library
(define-structure cogen-compile-skeleton cogen-skeleton-interface
  (open scheme auxiliary signals
	cogen-globals
	cogen-abssyn
	cogen-oca
	cogen-bta
	pgg-compiler-library)
  (files cogen-skeleton))
