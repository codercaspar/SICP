(define (make-point x y)
	(cons x y))
	
(define (x-point point)
	(car point))
	
(define (y-point point)
	(cdr point))

(define (make-rectangle1 x y)
	(cons x y))
	
(define (rect-width rect)
	(- (x-point (cdr rect)) (x-point (car rect))))
	
(define (rect-height rect)
	(- (y-point (car rect)) (y-point (cdr rect))))
	
(define (rect-perimeter r)
   (* 2 (+ (rect-width r) (rect-height r))))

(define (rect-area r)
   (* (rect-width r) (rect-height r)))