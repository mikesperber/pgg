;;; cogen-gensym.scm

;;; copyright © 1996-2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact


;;; symbol generation

(define *gensym-counter* (make-cell 0))

(define (gensym-reset! . rest)
  (set! *gensym-counter*
	(make-cell (if (and (pair? rest) (number? (car rest)))
		       (car rest)
		       0))))

(define (gensym sym)
  (atomically
   (provisional-cell-set! *gensym-counter*
			  (+ (provisional-cell-ref *gensym-counter*) 2))
   (any->symbol sym "-" (provisional-cell-ref *gensym-counter*))))

(define (gensym-global-trimmed sym)
  (gensym (trim-symbol sym)))

(define (gensym-global-ignore sym)
  (gensym 'f))

(define gensym-trimmed gensym-global-trimmed)

;; gensym-local

(define *gensym-local* (make-fluid '()))

(define (with-fresh-gensym-local thunk)
  (let-fluid *gensym-local*
	     (cons 1 (fluid *gensym-local*))
	     thunk))

(define (with-held-gensym-local hold thunk)
  (if (null? hold)
      (thunk)
      (let-fluid *gensym-local*
		 (cons (car hold) (fluid *gensym-local*))
		 thunk)))

(define (get-gensym-local)
  (fluid *gensym-local*))

(define (gensym-local-use-stub sym)
  (if (null? (get-gensym-local))
      (gensym sym)
      (begin
	(atomically
	 (provisional-set-car! (get-gensym-local)
			       (+ (provisional-car (get-gensym-local)) 2))
	 (any->symbol sym "-" (provisional-car (get-gensym-local)))))))



(define *gensym-local* (make-fluid '()))

(define (get-gensym-local) (fluid *gensym-local*))

(define (preserving-gensym-local thunk)
  (let ((old (get-gensym-local)))
    (lambda ()
      (let-fluid *gensym-local*
		 old
		 thunk))))

(define (gensym-local-hold)
  (get-gensym-local))

(define (gensym-local-use-stub sym)
  (if (null? (get-gensym-local))
      (gensym sym)
      (begin
	(atomically
	 (provisional-set-car! (get-gensym-local)
			       (+ (provisional-car (get-gensym-local)) 2))
	 (any->symbol sym "-" (provisional-car (get-gensym-local)))))))

(define (gensym-local-trimmed-use-stub sym)
  (gensym-local-use-stub (trim-symbol sym)))

(define (gensym-local-ignore-stub sym)
  (if (null? (get-gensym-local))
      (gensym "x")
      (begin
	(atomically
	 (provisional-set-car! (get-gensym-local)
			       (+ (provisional-car (get-gensym-local)) 2))
	 (any->symbol "x-" (provisional-car (get-gensym-local)))))))

(define (gensym-ignore-name-stubs!)
  (set! gensym-trimmed gensym-global-ignore)
  (set! gensym-local gensym-local-ignore-stub)
  (set! gensym-local-trimmed gensym-local-ignore-stub))

(define (gensym-use-name-stubs!)
  (set! gensym-trimmed gensym-global-trimmed)
  (set! gensym-local gensym-local-use-stub)
  (set! gensym-local-trimmed gensym-local-trimmed-use-stub))

(define gensym-local gensym-local-use-stub)
(define gensym-local-trimmed gensym-local-trimmed-use-stub)
