(define (make-center-percent c r)
	(make-interval (- c (* c r)) (+ c (* c r))))
	
(define (center i)
	(/ (+ (lower-bound i) (upper-bound i)) 2))
	
(define (rate i)
	(/ (- (upper-bound i) (center i)) (center i)))

(define (make-interval a b)
	(cons a b))
	
(define (upper-bound interval)
	(cdr interval))
	
(define (lower-bound interval)
	(car interval))