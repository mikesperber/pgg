; Digraph algorithm from Pennello/DeRemer
; =======================================

; This function is very generic, imperative, kludgy,
; and it has too many arguments

(define (complete-subsets! for-each-a a-equal? for-each-R
			   associate-depth! depth-association
			   overwrite! merge!)
  (let ((stack '())
	(depth 0))

    ;; #f means infinity
    (define (depth-min a b)
      (cond ((not a) b)
	    ((not b) a)
	    (else (min a b))))

    (define (descend! a)
      (set! stack (cons a stack))
      (set! depth (+ 1 depth))
      (let ((depth depth))
	(associate-depth! a depth)
	(for-each-R
	 (lambda (b)
	   (if (eqv? 0 (depth-association b)) ; can't use zero? 'cause it may be #f
	       (descend! b))
	   (associate-depth! a
			     (depth-min (depth-association a)
					(depth-association b)))
	   (merge! a b))
	 a)
      
	(if (= (depth-association a) depth)
	    (let loop ()
	      (let ((top (car stack)))
		(associate-depth! top #f)
		(overwrite! top a)
		(set! stack (cdr stack))
		(if (not (a-equal? top a))
		    (loop)))))))

    (for-each-a (lambda (a)
		  (if (eqv? 0 (depth-association a))
		      (descend! a))))))
