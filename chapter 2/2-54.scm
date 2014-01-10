(define (my-equal? list1 list2)
	(cond ((null? list1) #t)
		  ((eq? (car list1) (car list2)) (my-equal? (cdr list1) (cdr list2)))
		  (else #f)))