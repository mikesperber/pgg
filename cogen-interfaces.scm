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
	  annMakeEval
	  annIsEval?
	  annFetchEvalBody
	  annFetchEvalDiff
	  annSetEvalDiff!
	  annFetchEvalQuoted
	  annSetEvalQuoted!
	  annIntroduceLift
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
	  make-ge-eval))

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

(define-interface cogen-ctor-interface
  (export ((define-data) :syntax)))

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

(define-interface cogen-anf-compile-interface
  (export _vlambda
	  start-memo-internal
	  multi-memo
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
	    _var
	    start-memo)
	   :syntax)))

(define-interface cogen-directives-interface
  (export ((define-without-memoization
	     define-memo
	     define-primitive)
	   :syntax)))

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
	cogen-globals 
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
	  project-static
	  project-dynamic
	  project-dynamic-level
	  clone-one-dynamic
	  clone-dynamic
	  clone-with
	  multi-append
	  binding-times))

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

(define-interface cogen-typesig-interface
  (export parse-type desc-type desc-np desc-nc desc-nt))

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
	  *memo-optimize*
	  *generating-extension*
	  set-bta-diplay-level!
	  set-effect-display-level!
	  set-scheme->abssyn-static-references!
	  set-scheme->abssyn-label-counter!
	  set-scheme->abssyn-let-insertion!
	  set-memo-optimize!
	  set-generating-extension!
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
	  labset-union
	  labset-union*
	  labset-subtract
	  labset-subset?
	  labset-equal?
	  labset-for-each))

(define-interface pgg-interface
  (export cogen-driver))

(define-interface pretty-print-interface
  (export p))

(define-interface auxiliary-interface
  (export id succ pred
	  gensym gensym-reset!
	  gensym-local gensym-local-reset! gensym-local-push!
	  gensym-local-pop!
	  *memolist* *residual-program* *support-code*
	  add-to-memolist! clear-memolist!
	  set-residual-program! add-to-residual-program! clear-residual-program!
	  add-to-support-code! clear-support-code!
	  nlist
	  set-union set-subtract set-difference set-union* set-equal?
	  and-map and-map2 strict-and-map strict-or-map
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

(define-structure cogen-abssyn cogen-abssyn-interface
  (open scheme signals auxiliary)
  (files cogen-abssyn))

(define-structure cogen-labset cogen-labset-interface
  (open scheme signals)
  (files cogen-labset))

(define-structure pgg pgg-interface
  (open scheme auxiliary
	cogen-scheme
	cogen-bta
	cogen-skeleton)
  (files cogen-driver))

(define-structure pgg-library
  (compound-interface cogen-construct-genext-interface
		      cogen-residual-interface
		      cogen-direct-syntax-interface)
  (open scheme escapes signals auxiliary cogen-globals)
  (files shift-reset
	 cogen-library
	 cogen-residual
	 cogen-direct-anf))

(define-structure pgg-residual
  (export ((start-memo define-data) :syntax)
	  nextlevel
	  make-cell cell-ref cell-set!)
  (open scheme escapes pgg-library)
  (files cogen-boxops
	 cogen-ctors))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-structure pgg-compiler-library
  (compound-interface cogen-construct-genext-interface
		      anf-specializer-interface
		      cogen-anf-compile-interface)
  (open scheme escapes signals auxiliary cogen-globals
	anf-specializer)
  (files
	 cogen-boxops
	 cogen-ctors
	 cogen-library
	 shift-reset
	 cogen-anf-compile
	 ))

(define-structure pgg-compiler
  (export cogen-driver)
  (open scheme
	escapes signals pretty-print
	anf-specializer
	cogen-record)
  (files 
	 auxiliary
	 cogen-globals
	 cogen-labset
	 cogen-env
	 cogen-typesig
	 cogen-abssyn
	 cogen-scheme
	 cogen-oca
	 cogen-skeleton
	 cogen-effect
	 cogen-eq-flow
	 ;; to run the generating extension
	 cogen-boxops
	 cogen-ctors
	 cogen-library
	 shift-reset
	 cogen-anf-compile
	 cogen-driver
	 ))


