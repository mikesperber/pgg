;;; global variables for the PGG system

(define *bta-display-level* 1)		;debug level for the BTA
(define *effect-display-level* 1)	;debug level for the effect analysis

(define *scheme->abssyn-static-references* #f)
(define *scheme->abssyn-label-counter* 0)

(define *abssyn-maybe-coerce* #t)
(define *scheme->abssyn-let-insertion* #t)
(define *memo-optimize* #t)		;use representation analysis
(define *generating-extension* '())

(define *termination-analysis* #f)	;do not run termination analysis

;;; setter functions

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

(define (set-abssyn-maybe-coerce! v)
  (set! *abssyn-maybe-coerce* v))

(define (set-termination-analysis! v)
  (set! *termination-analysis* v))
