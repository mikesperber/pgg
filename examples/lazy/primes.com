(stream-filter p l = stream-filter-aux p (lazy-car l) (lazy-cdr l))
(stream-filter-aux p x xs = if (p x)
	                    (lazy-cons x (stream-filter p xs))
			    (stream-filter p xs))

(drop-multiples xs = drop-multiples-aux (lazy-car xs) (lazy-cdr xs))
(drop-multiples-aux x xs = lazy-cons x (stream-filter (pred x) xs))

(pred p x = /= 0 (mod x p))

(from n = lazy-cons n (from (+ n 1)))

(primes = drop-multiples (from 2))

(first-n n l  = if (= n 0)
		'()
		(cons (lazy-car l) (first-n (- n 1) (lazy-cdr l))))

(lazy-cons x y z = z x y)
(lazy-car x	 = x fst)
(lazy-cdr x	 = x snd)

(fst x y = x)
(snd x y = y)

(goal input = first-n input primes)
