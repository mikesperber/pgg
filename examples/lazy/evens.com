; Similix Lazy combinator source program example
; Copyright (C) 1993 Anders Bondorf
; Please see the file README for copyright notice, license and disclaimer.
; Changes by Peter Thiemann 1996

;-----------------------------------------------------------------------------
(first-n n l  = if (= n 0)
		'()
		(cons (lazy-car l) (first-n (- n 1) (lazy-cdr l))))

(evens-from n = lazy-cons n (evens-from (+ n 2)))

(lazy-cons x y z = z x y)
(lazy-car x	 = x fst)
(lazy-cdr x	 = x snd)

(fst x y = x)
(snd x y = y)

(goal input = first-n input (evens-from 0))

;-----------------------------------------------------------------------------
