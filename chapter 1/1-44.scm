(define (repeated f n)
	(if (= n 1)
		(lambda (x) (f x))
		(lambda (x) ((mycompose f (repeated f (- n 1))) x))))

(define (mycompose f g)
	(lambda (x) (f (g x))))

(define (average x y z) (/ (+ x y z) 3))

(define (smooth f)
	(define dx 0.001)
	(lambda (x) (average (f x) (f (+ x dx)) (f (- x dx)))))

(define (n-fold-smooth f n)
	(repeated (smooth f) n))