;;; pgg-packages.scm

;;; copyright © 1996-2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

(define-interface cogen-abssyn-interface
  (export annMakeDef
	  annMakeDefMutable
	  annMakeDefWithoutMemoization
	  annIsDef?
	  annIsDefMutable?
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
	  annFetchVarGlobal
	  annSetVarGlobal!
	  annFetchVarCall
	  annSetVarCall!
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
	  annMakePureOp
	  annMakeFullOp
	  annMakeOp1
	  annMakeOpCoerce
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
	  annMakeBegin
	  annIsBegin?
	  annFetchBeginHeader
	  annFetchBeginBody
	  annFetchBeginUnfoldability
	  annSetBeginUnfoldability!
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
	  annFetchLambdaPoly
	  annSetLambdaPoly!
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
	  annFetchSelCtor
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
	  annMakeVector
	  annIsVector?
	  annFetchVectorLabel
	  annFetchVectorSize
	  annFetchVectorArg
	  annMakeVref
	  annIsVref?
	  annFetchVrefArg
	  annFetchVrefIndex
	  annMakeVlen
	  annIsVlen?
	  annFetchVlenVec
	  annMakeVset
	  annIsVset?
	  annFetchVsetLabel
	  annFetchVsetVec
	  annFetchVsetIndex
	  annFetchVsetArg
	  annMakeVfill
	  annIsVfill?
	  annFetchVfillLabel
	  annFetchVfillVec
	  annFetchVfillArg
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
	  annFetchMemoSpecial
	  ann-replace
	  ann-maybe-coerce
	  annFreeVars
	  annExprTerminates?
	  ann-dsp-e ann-dsp-d*))

(define-interface cogen-construct-genext-interface
  (export make-ge-var
	  make-ge-freevar
	  make-ge-const
	  make-ge-cond
	  with-primitive-op?
	  make-ge-op
	  make-ge-op-pure
	  make-ge-call
	  make-ge-let
	  make-ge-begin
	  make-ge-lambda-memo
	  make-ge-vlambda-memo
	  make-ge-app-memo
	  make-ge-lambda
	  make-ge-vlambda
	  make-ge-app
	  make-ge-ctor-memo
	  make-ge-sel-memo
	  make-ge-test-memo
	  make-ge-ctor
	  make-ge-sel
	  make-ge-test
	  make-ge-lift
	  make-ge-eval
	  make-ge-run
	  make-ge-make-vector-memo
	  make-ge-vector-ref-memo
	  make-ge-vector-length-memo
	  make-ge-vector-set!-memo
	  make-ge-vector-fill!-memo
	  make-ge-make-cell-memo
	  make-ge-cell-ref-memo
	  make-ge-cell-set!-memo
	  make-ge-cell-eq?-memo))

(define-structure cogen-construct-genext cogen-construct-genext-interface
  (open scheme
	fluids
	cogen-residual cogen-globals)
  (files cogen-construct-genext))

(define-interface cogen-directives-interface
  (export ((define-without-memoization
	     define-memo
	     define-primitive)
	   :syntax)))

(define-structure cogen-directives cogen-directives-interface
  (open scheme)
  (files cogen-directives))

(define-interface cogen-terminate-interface
  (export perform-termination-analysis))

(define-interface cogen-driver-interface
  (export cogen-driver))

(define-interface cogen-env-interface
  (export the-empty-env
	  extend-env
	  extend-env*
	  apply-env
	  map-env
	  for-each-env!
	  
	  empty-boxed-env
	  extend-boxed-env
	  extend-boxed-env*
	  fresh-boxed-env*
	  apply-boxed-env
	  shrink-boxed-env
	  make-boxed-env
	  unbox-env))

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

(define-interface cogen-effect-interface
  (export *effect-display-level*
	  effect-analysis
	  effect-label->type
	  effect-for-each
	  effect->labset))

(define-interface cogen-oca-interface
  (export oca-d))

(define-structure cogen-oca cogen-oca-interface
  (open scheme signals
	auxiliary
	cogen-abssyn)
  (files cogen-oca))

(define-interface cogen-typesig-interface
  (export process-type-declarations parse-type
	  desc-type desc-ctor desc-np desc-nc desc-nt desc-hidden
	  type-var? type-var->tvar
	  type-all? type-all->tvar type-all->type
	  type-rec? type-rec->tvar type-rec->type
	  type-app? type-app->tcon type-app->types))

(define-structures ((cogen-bta cogen-bta-interface)
		    (cogen-typesig cogen-typesig-interface))
  (open scheme signals auxiliary
	cogen-scheme cogen-gensym cogen-globals pp cogen-env
	cogen-abssyn cogen-record cogen-labset)
  (files cogen-eq-flow
	 cogen-effect
	 cogen-typesig))

(define-interface cogen-macro-interface
  (export syntax-make-pop-mark
	  syntax-make-env-mark
	  syntax-marked-exp
	  syntax-marked-env
	  syntax-pop-mark?
	  syntax-null?
	  syntax-pair?
	  syntax-car
	  syntax-cdr
	  syntax-map
	  syntax-depth
	  syntax-strip
	  syntax-strip-recursively
	  syntax-eq?
	  syntax-eq-symbol?
	  parse-syntax-rules
	  syntax-rules-transformer))

(define-structure cogen-macro cogen-macro-interface
  (open scheme signals auxiliary
	cogen-env)
  (files cogen-macro))

(define-interface scheme-desugar-interface
  (export desugar))

(define-structure scheme-desugar scheme-desugar-interface
  (open scheme filenames source-file-names auxiliary
	cogen-scheme)
  (files scheme-desugar))

(define-interface cogen-scheme-interface
  (export scheme->abssyn-d
	  scheme->abssyn-make-call
	  scheme->abssyn-make-ctor1
	  scheme-desugar))

(define-structure cogen-scheme cogen-scheme-interface
  (open scheme auxiliary signals
	cogen-gensym cogen-globals cogen-macro
	cogen-abssyn cogen-env cogen-record)
  (files cogen-scheme))

(define-interface cogen-skeleton-interface
  (export generate-d
	  *generating-extension*))

(define-structure cogen-skeleton cogen-skeleton-interface
  (open scheme auxiliary signals
	cogen-gensym
	cogen-globals
	cogen-abssyn
	cogen-oca
	cogen-bta
	cogen-typesig
	cogen-construct-genext)
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

(define-structure cogen-terminate cogen-terminate-interface
  (open scheme auxiliary signals
	cogen-globals cogen-abssyn cogen-env cogen-record)
  (files cogen-terminate))

(define-structure cogen-abssyn cogen-abssyn-interface
  (open scheme signals auxiliary cogen-globals)
  (files cogen-abssyn))

(define-structure cogen-labset cogen-labset-interface
  (open scheme signals)
  (files cogen-labset-bylist))

(define-structure cogen-labset-bylist cogen-labset-interface
  (open scheme signals)
  (files cogen-labset-bylist))

(define-structure pgg pgg-interface
  (open scheme filenames source-file-names auxiliary signals
	pp
	cogen-scheme
	cogen-typesig
	cogen-bta
	cogen-skeleton
	cogen-terminate)
  (files cogen-driver))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-structure reaching-definitions pgg-interface
  (open scheme signals auxiliary pp
	cogen-abssyn cogen-env cogen-globals cogen-labset cogen-record
	cogen-typesig 
	cogen-scheme)
  (files cogen-reach))

