;;; finite maps: symbol -> anything
;;; open features

(define finite-map-size 3)
(define (symbol-hash key size)
  (remainder (string-hash (symbol->string key)) size))
(define (symbol<? k1 k2)
  (string<? (symbol->string k1) (symbol->string k2)))

(define make-symbol-fm
  (lambda (alist)
    (make-fm symbol-hash symbol<? alist)))
(define make-number-fm
  (lambda (alist)
    (make-fm remainder < alist)))

(define-record finite-map
  (hash key< vec size card))

(define make-fm
  (lambda (hash key< alist)
    (let ((card (length alist)))
      (let ((size (+ finite-map-size (* 2 card))))
	(let ((vec (make-vector size '())))
	  (for-each (lambda (key-value)
		      (let ((key (car key-value))
			    (value (cdr key-value)))
			(let* ((i (hash key size))
			       (bucket (vector-ref vec i)))
			  (vector-set! vec i (bucket-insert key< key value bucket
							    (lambda ()
							      'nothing-to-do))))))
		    alist)
	  (make-finite-map hash key< vec size card))))))

(define bucket-insert
  (lambda (key< key value bucket new)
    (let loop ((bucket bucket))
      (if (null? bucket)
	  (list (cons key value))
	  (let ((first-key (caar bucket)))
	    (cond
	     ((key< key first-key)
	      (new)
	      (cons (cons key value) bucket))
	     ((key< first-key key)
	      (new)
	      (cons (car bucket) (loop (cdr bucket))))
	     (else
	      (cons (cons key value) (cdr bucket)))))))))

(define fm-reorganize
  (lambda (fm new-size)
    (let ((vec (make-vector new-size '()))
	  (hash (finite-map->hash fm))
	  (key< (finite-map->key< fm)))
      (fm-walk fm
	       (lambda (key value)
		 (let* ((i (hash key new-size))
			(bucket (vector-ref vec i)))
		   (vector-set! vec i (bucket-insert key< key value bucket
						     (lambda ()
						       'nothing-to-do))))))
      (finite-map->vec! fm vec)
      (finite-map->size! fm new-size))))

(define fm->alist
  (lambda (fm)
    (let ((l (vector-length fm)))
      (let loop ((i 0) (result '()))
	(if (< i l)
	    (loop (+ i 1)
		  (append (vector-ref fm i) result))
	    result)))))

(define fm-join				;broken
  (lambda (fm1 fm2 join)
    (let ((l finite-map-size)
	  (vec (make-vector finite-map-size))
	  (vec1 (finite-map->vec fm1))
	  (vec2 (finite-map->vec fm2)))
      (let loop ((i 0))
	(if (< i l)
	    (begin
	      (vector-set! vec i (fm-join-alist (finite-map->key< fm1)
						(vector-ref vec1 i)
						(vector-ref vec2 i)
						join))
	      (loop (+ i 1)))
	    vec)))))

(define fm-join-alist
  (lambda (key< al1 al2 join)
    (let loop ((al1 al1) (al2 al2))
      (cond
       ((null? al1)
	al2)
       ((null? al2)
	al1)
       (else
	(let ((kv1 (car al1))
	      (kv2 (car al2)))
	  (let ((k1 (car kv1))
		(k2 (car kv2)))
	    (cond
	     ((key< k1 k2)
	      (cons kv1 (loop (cdr al1) al2)))
	     ((key< k2 k1)
	      (cons kv2 (loop al1 (cdr al2))))
	     (else
	      (cons (cons k1 (join (cdr kv1) (cdr kv2)))
		    (loop (cdr al1) (cdr al2))))))))))))

(define fm-dom
  (lambda (fm)
    (let ((l (vector-length fm)))
      (let loop ((i 0) (result '()))
	(if (< i l)
	    (loop (+ i 1)
		  (append (map car (vector-ref fm i)) result)))))))

(define fm-restrict			;broken
  (lambda (fm new-dom)
    (let ((vec (finite-map->vec fm))
	  (newvec (make-vector finite-map-size)))
      (let loop ((i 0))
	(if (< i finite-map-size)
	    (begin
	      (vector-set! newvec i (filter (lambda (kv) (memq (car kv) new-dom))
					    (vector-ref vec i)))
	      (loop (+ i 1)))
	    (make-finite-map (finite-map->hash fm)
			     (finite-map->key< fm)
			     newvec))))))

(define fm-lookup
  (lambda (fm key)
    (let* ((i ((finite-map->hash fm) key (finite-map->size fm)))
	   (bucket (vector-ref (finite-map->vec fm) i)))
      (assq key bucket))))

(define fm-update!
  (lambda (fm key value)
    (let* ((size (finite-map->size fm))
	   (i ((finite-map->hash fm) key size))
	   (vec (finite-map->vec fm))
	   (bucket (vector-ref vec i))
	   (overflow #f)
	   (new (lambda ()
		  (let ((card (+ 1 (finite-map->card fm))))
		    (finite-map->card! fm card)
		    (if (> card (* 2 size))
			(set! overflow card))))))
      (vector-set! vec i (bucket-insert (finite-map->key< fm) key value bucket new))
      (if overflow (fm-reorganize fm overflow)))))

(define fm-walk
  (lambda (fm proc)			;proc : key x value -> void
    (let* ((vec (finite-map->vec fm))
	   (l (vector-length vec)))
      (let loop ((i 0))
	(if (< i l)
	    (let ((bucket (vector-ref vec i)))
	      (for-each (lambda (k-v) (proc (car k-v) (cdr k-v))) bucket)
	      (loop (+ i 1))))))))
