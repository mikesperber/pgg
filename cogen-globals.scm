;;; cogen-globals.scm

;;; copyright © 1996, 1997, 1998, 1999 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; global variables for the PGG system

(define *bta-display-level* 1)		;debug level for the BTA
(define *effect-display-level* 1)	;debug level for the effect analysis

(define *scheme->abssyn-static-references* #f)
(define *scheme->abssyn-label-counter* 0)

(define *abssyn-maybe-coerce* #t)
(define *scheme->abssyn-let-insertion* #t)
(define *memo-optimize* #t)		;use representation analysis
(define *generating-extension* '())
(define *memolist-stages* 0)		;stages in memoization table

(define *termination-analysis* #f)	;do not run termination analysis

(define *generate-flat-program* #f)	;do not create a flat program

(define *lambda-is-pure* #t)		;consider lambda as pure

(define *lambda-is-toplevel* #f)	;generate a toplevel function for each memoized lambda

;;; setter functions

(define (set-generate-flat-program! b)
  (set! *generate-flat-program* b))

(define (set-bta-display-level! n)
  (set! *bta-display-level* n))
(define (set-effect-display-level! n)
  (set! *effect-display-level* n))

(define (set-scheme->abssyn-static-references! v)
  (set! *scheme->abssyn-static-references* v))
(define (set-scheme->abssyn-label-counter! n)
  (set! *scheme->abssyn-label-counter* n))

(define (set-scheme->abssyn-let-insertion! v)
  (set! *scheme->abssyn-let-insertion* v))
(define (set-memo-optimize! v)
  (set! *memo-optimize* v))
(define (set-generating-extension! v)
  (set! *generating-extension* v))
(define (set-memolist-stages! n)
  (if (and (number? n) (<= 0 n))
      (set! *memolist-stages* n)))

(define (set-abssyn-maybe-coerce! v)
  (set! *abssyn-maybe-coerce* v))

(define (set-termination-analysis! v)
  (set! *termination-analysis* v))

(define (set-lambda-is-pure! v)
  (set! *lambda-is-pure* v))

(define (set-lambda-is-toplevel! v)
  (set! *lambda-is-toplevel* v))
