(define (test a choice)
	(if (= choice 1)
		(lambda () (set! a 20))
		(lambda () (display a))))