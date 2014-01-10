(define (same-parity x . y)
	(define (filter l r)
		(cond ((null? l) l)
			  ((= r (remainder (car l) 2)) (append (list (car l)) (filter (cdr l) r)))
			  (else (filter (cdr l) r))))
	(if (null? y)
		x
		(append (list x) (filter y (remainder x 2)))))