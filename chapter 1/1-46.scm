(define (fix-point f first-guess)
	((iterative-improve close-enough? f) first-guess))
	
(define (iterative-improve close-enough? next-guess)
		(lambda (x)
	    (let ((next (next-guess x)))
			    (cond ((close-enough? x next) next)
				      (else ((iterative-improve close-enough? next-guess) next))))))
	
(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) 0.000001))

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
	
(define (square-root x)
	(fixed-point-of-transfrom (lambda (y) (/ x y)) average-damp 1.0))