(define (deriv g)
	(define dx 0.00001)
	(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
	(lambda (x) (- x (/ (g x) ((deriv g) x)))))
	
(define (average-transform g)
	(lambda (x) (/ (+ x (g x)) 2)))

(define (fixed-point-of-transfrom g transform guess)
	(fix-point (transform g) guess))

(define (fix-point f first-guess)
	(define tolerance 0.000001)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
	    (newline)
		(display guess)
		(let ((next-guess (f guess)))
			(if (close-enough? guess next-guess)
				next-guess
				(try next-guess))))
	(try first-guess))

(define (square-root x)
	(fixed-point-of-transfrom (lambda (y) (- x (* y y))) newton-transform 1.0))
	
(define (square-root-average x)
	(fixed-point-of-transfrom (lambda (y) (/ x y)) average-transform 1.0))

(define (cubic a b c)
	(fixed-point-of-transfrom (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
		                      newton-transform
		                      1.0))
							  
(define (test)
	(fixed-point-of-transfrom (lambda (x) (+ (* x x x) (* -3 x x) (+ 3 x) (- 0 0.95)))
		                      newton-transform
		                      9))