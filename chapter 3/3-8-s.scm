(define (t x)
	(lambda (y)
		(if x
			(if (= y 0)
				1
				(begin (set! x #f) 0))
			0)))
			
(define f (t #t))

(+ (f 0) (f 1))