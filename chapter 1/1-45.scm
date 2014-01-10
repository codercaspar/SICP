(define (repeated f n)
	(if (= n 1)
		(lambda (x) (f x))
		(lambda (x) ((mycompose f (repeated f (- n 1))) x))))

(define (mycompose f g)
	(lambda (x) (f (g x))))

(define (fix-point f first-guess)
	(define tolerance 0.000001)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next-guess (f guess)))
			(if (close-enough? guess next-guess)
				next-guess
				(try next-guess))))
	(try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
	(lambda (x) (average x (f x))))

(define (fixed-point-of-transfrom g transform guess)
	(fix-point (transform g) guess))

(define (fourth-root x)
	(fixed-point-of-transfrom (lambda (y) (/ x (* y y y))) (repeated average-damp 2) 1.0))

(define (fifth-root x)
	(fixed-point-of-transfrom (lambda (y) (/ x (* y y y y))) (repeated average-damp 2) 1.0))

(define (sixth-root x)
	(fixed-point-of-transfrom (lambda (y) (/ x (* y y y y y))) (repeated average-damp 2) 1.0))

(define (seventh-root x)
	(fixed-point-of-transfrom (lambda (y) (/ x (* y y y y y y))) (repeated average-damp 2) 1.0))

(define (eighth-root x)
	(fixed-point-of-transfrom (lambda (y) (/ x (* y y y y y y y))) (repeated average-damp 3) 1.0))
	
(define (cube-root x)
	(fixed-point-of-transfrom (lambda (y) (/ x (* y y))) average-damp 1.0))

(define (square-root x)
	(fixed-point-of-transfrom (lambda (y) (/ x y)) average-damp 1.0))