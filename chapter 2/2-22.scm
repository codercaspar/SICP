(define (square-list items)
	(define (square x) (* x x))
	(define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things)
				  (cons (square (car things))
						answer))))
	(iter items (list)))
	
(define (square-list1 items)
	(define (square x) (* x x))
	(define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things)
				  (cons answer
				       (square (car things))))))
	(iter items (list)))
	
(define (square-list2 items)
	(define (square x) (* x x))
	(define (iter things answer)
		(if (null? things)
			answer
			(iter (cdr things)
				  (append answer
						  (list (square (car things)))))))
	(iter items (list)))