(define (myreverse l)
	(define (myreverse-iter cl r)
			(if (null? cl)
				r
				(myreverse-iter (cdr cl) (append (list (car cl)) r))))
	(> (square-list (list 1 2 3 4))
(16 9 4 1)
> (square-list1 (list 1 2 3 4))
((((() . 1) . 4) . 9) . 16)
> (square-list2 (list 1 2 3 4))
(1 4 9 16)
> -iter l (list)))