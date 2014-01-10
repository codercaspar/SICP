(define (square-list1 items)
	(define (square x) (* x x))
	(if (null? items)
		nil
		(cons (square (car items)) (square-list (cdr items)))))
		
(define (square-list2 items)
	(map (lambda (x) (* x x)) items))