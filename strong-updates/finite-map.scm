;;; finite maps: symbol -> anything
;;; open features

(define finite-map-size 15)
(define (symbol-hash key)
  (remainder (string-hash (symbol->string key)) finite-map-size))
(define (symbol<? k1 k2)
  (string<? (symbol->string k1) (symbol->string k2)))

(define make-symbol-fm
  (lambda (alist)
    (make-fm symbol-hash symbol<? alist)))
(define make-number-fm
  (lambda (alist)
    (make-fm (lambda (z) z) < alist)))

(define-record finite-map
  (hash key< vec))

(define make-fm
  (lambda (hash key< alist)
    (let ((vec (make-vector finite-map-size '())))
      (for-each (lambda (key-value)
		  (let ((key (car key-value))
			(value (cdr key-value)))
		    (let* ((i (hash key))
			   (bucket (vector-ref vec i)))
		      (vector-set! vec i (bucket-insert key< key value bucket)))))
		alist)
      (make-finite-map hash key< vec))))

(define bucket-insert
  (lambda (key< key value bucket)
    (let loop ((bucket bucket))
      (if (null? bucket)
	  (list (cons key value))
	  (let ((first-key (caar bucket)))
	    (cond
	     ((key< key first-key)
	      (cons (cons key value) bucket))
	     ((key< first-key key)
	      (cons (car bucket) (loop (cdr bucket))))
	     (else
	      (cons (cons key value) (cdr bucket)))))))))

(define fm->alist
  (lambda (fm)
    (let ((l (vector-length fm)))
      (let loop ((i 0) (result '()))
	(if (< i l)
	    (loop (+ i 1)
		  (append (vector-ref fm i) result))
	    result)))))

(define fm-join
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

(define fm-restrict
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
    (let* ((i ((finite-map->hash fm) key))
	   (bucket (vector-ref (finite-map->vec fm) i)))
      (assq key bucket))))

(define fm-update!
  (lambda (fm key value)
    (let* ((i ((finite-map->hash fm) key))
	   (vec (finite-map->vec fm))
	   (bucket (vector-ref vec i)))
      (vector-set! vec i (bucket-insert (finite-map->key< fm) key value bucket)))))

(define fm-walk
  (lambda (fm proc)			;proc : key x value -> void
    (let* ((vec (finite-map->vec fm))
	   (l (vector-length vec)))
      (let loop ((i 0))
	(if (< i l)
	    (let ((bucket (vector-ref vec i)))
	      (for-each (lambda (k-v) (proc (car k-v) (cdr k-v))) bucket)
	      (loop (+ i 1))))))))
