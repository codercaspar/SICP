(define (make-accumulate sum)
	(lambda (add)
		(begin (set! sum (+ sum add))
		sum)))
		
(define A (make-accumulate 5))

(A 10)
