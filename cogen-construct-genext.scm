;;; cogen-construct-genext.scm

;;; copyright © 1996-2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; interface to create generating extensions
;;; syntax constructors
(define (make-ge-var l v)
  v)
(define (make-ge-freevar l v)
  `(_FREEVAR ,l ,v))
(define (make-ge-const c)
  (if (number? c)
      c
      `',c))
(define (make-ge-cond l lb c t e)
  (if (zero? l)
      (make-residual-if c t e)
      `(_IF ,l ,c ,t ,e)))

(define $primitive-op? (make-fluid (lambda (op) #t)))

(define (with-primitive-op? op? thunk)
  (let-fluid $primitive-op? op? thunk))

(define (primitive-op? o)
  ((fluid $primitive-op?) o))

;; "OP" is really a misnomer: this is about *external* procedures,
;; only some of which are primitive ops
(define (make-ge-op l o args)
  (if (primitive-op? o)
      `(_OP ,l ,o ,@args)
      `(_OP-SERIOUS ,l ,o ,@args)))
(define (make-ge-op-pure l o args)
  (cond
   ((zero? l) (cons o args))
   ((primitive-op? o) `(_OP_PURE ,l ,o ,@args))
   (else `(_OP ,l ,o ,@args))))

(define (make-ge-call f bts args)
  (apply make-residual-call f args))
(define (make-ge-let hl unf? bl v e body)
  (make-residual-let v e body))
(define (make-ge-begin hl prop? e1 e2)
  (if (zero? hl)
      (make-residual-begin e1 e2)
      `(_BEGIN ,hl ,prop? ,e1 ,e2)))
(define (make-ge-lambda-memo l poly? vars btv body-level label fvars bts body)
  (cond
   (poly?
    `(_LAMBDA_POLY ,l ,vars ',btv ,body-level ',label ,body))
   (*lambda-is-toplevel*
    (let ((name (string->symbol (string-append "lambda-" (number->string label)))))
      (set-generating-extension!
       (cons `(DEFINE ,name (LAMBDA ,fvars (LAMBDA ,vars ,body)))
	     *generating-extension*))
      `(_LAMBDA_MEMO ,l ',vars ',name (LIST ,@fvars) ',bts
		     ,name)))
   (else
    `(_LAMBDA_MEMO ,l ',vars ',label (LIST ,@fvars) ',bts
		   (LAMBDA ,fvars (LAMBDA ,vars ,body))))))
(define (make-ge-vlambda-memo l fixed-vars var btv label fvars bts body)
  `(_VLAMBDA_MEMO ,l ',fixed-vars ',var ',label (LIST ,@fvars) ',bts
		  (LAMBDA ,fvars
		    (LAMBDA ,(if (zero? l)
				 (append fixed-vars var)
				 (cons var fixed-vars)) ,body))))
(define (make-ge-app-memo l f btv args)
  `(_APP_MEMO ,l ,f ,@args))
(define (make-ge-lambda l vars btv fvs bts body)
  (if (zero? l)
      (make-residual-closed-lambda vars '() body)
      `(_LAMBDA ,l ,vars (LIST ,@fvs) ',bts ,body)))
(define (make-ge-vlambda l fixed-vars var btv body)
  (if (zero? l)
      (make-residual-closed-lambda (append fixed-vars var) '() body)
      `(_VLAMBDA ,l ,fixed-vars ,var ,body)))
(define (make-ge-app l f btv args)
  (if (zero? l)
      (apply make-residual-call f args)
      `(_APP ,l ,f ,@args)))
(define (make-ge-ctor-memo l bts hidden ctor args)
  `(_CTOR_MEMO ,l ,bts ,hidden ,ctor ,@args))
(define (make-ge-sel-memo l sel a)
  `(_S_T_MEMO ,l ,sel ,a))
(define (make-ge-test-memo l tst a)
  `(_S_T_MEMO ,l ,tst ,a))
(define (make-ge-ctor l ctor args)
  `(_OP ,l ,ctor ,@args))
(define (make-ge-sel l sel a)
  `(_OP ,l ,sel ,a))
(define (make-ge-test l tst a)
  `(_OP ,l ,tst ,a))
(define (make-ge-lift l diff a)
  `(_LIFT ,l ,diff ,a))
(define (make-ge-eval l diff a)
  `(_EVAL ,l ,diff ,a))
(define (make-ge-run l a)
  `(_RUN ,l ,a))
(define (make-ge-make-cell-memo l label bt a)
  `(_MAKE-CELL_MEMO ,l ,label ,bt ,a))
(define (make-ge-cell-ref-memo l a)
  `(_S_T_MEMO ,l CELL-REF ,a))
(define (make-ge-cell-set!-memo l r a)
  `(_MESSAGE!_MEMO ,l ,r CELL-SET! ,a))
(define (make-ge-cell-eq?-memo l args)
  `(_CELL-EQ?_MEMO ,l ,@args))
(define (make-ge-make-vector-memo l label bt s a)
  `(_MAKE-VECTOR_MEMO ,l ,label ,bt ,s ,a))
(define (make-ge-vector-ref-memo l v i)
  `(_S_T_MEMO ,l VECTOR-REF ,v ,i))
(define (make-ge-vector-length-memo l v)
  `(_S_T_MEMO ,l VECTOR-REF ,v))
(define (make-ge-vector-set!-memo l v i x)
  `(_MESSAGE!_MEMO ,l ,v VECTOR-SET! ,i ,x))
(define (make-ge-vector-fill!-memo l v x)
  `(_MESSAGE!_MEMO ,l ,v VECTOR-FILL! ,x))
