(define (repeated f n)
	(if (= n 1)
		(lambda (x) (f x))
		(lambda (x) ((mycompose f (repeated f (- n 1))) x))))

(define (mycompose f g)
	(lambda (x) (f (g x))))