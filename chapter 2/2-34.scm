(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))
			
(define (horner-eval x sequence)
	(accumulate (lambda (this high) (+ this (* high x))) 
				0
				sequence))