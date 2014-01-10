(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest))
				  (cdr rest))))
	(iter initial sequence))

(define (fold-right op initial sequence)
	(accumulate op initial sequence))	

(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))