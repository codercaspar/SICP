(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))
			
(define (mymap p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) (list) sequence))
	
(define (myappend seq1 seq2)
	(accumulate cons seq2 seq1))
	
(define (mylength sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence))