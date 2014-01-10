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

(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
				   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
	(add-interval x 
				  (make-interval (- 0 (upper-bound y))
								 (- 0 (lower-bound y)))))

(define (mul-interval x y)
	(make-center-percent (* (center x) (center y)) (+ (rate x) (rate y))))

(define (div-interval x y)
	(mul-interval x 
				  (make-interval (min (/ 1.0 (lower-bound y)) (/ 1.0 (upper-bound y)))
								 (max (/ 1.0 (lower-bound y)) (/ 1.0 (upper-bound y))))))

(define (par1 r1 r2)
	(div-interval (mul-interval r1 r2)
				  (add-interval r1 r2)))

(define (par2 r1 r2)
	(let ((one (make-interval 1 1)))
		(div-interval one
					  (add-interval (div-interval one r1)
					  				(div-interval one r2)))))