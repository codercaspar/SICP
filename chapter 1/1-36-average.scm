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

(define (average x y) (/ (+ x y) 2))

(define find (fix-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0))