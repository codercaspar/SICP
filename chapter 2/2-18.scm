(define (myreverse l)
	(if (null? l)
		(list)
		(append (myreverse (cdr l)) (list (car l)))))