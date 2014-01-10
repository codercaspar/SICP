(define (my-for-each f items)
	(cond ((null? items) #t)
		  (else (f (car items)) 
				(my-for-each f (cdr items)))))	