(define (make-point x y)
	(cons x y))
	
(define (x-point point)
	(car point))
	
(define (y-point point)
	(cdr point))

(define (make-rect p w h)
	(cons p (cons w h)))
	
(define (rect-width rect)
	(car (cdr rect)))
	
(define (rect-height rect)
	(cdr (cdr rect)))
	
(define (rect-perimeter r)
   (* 2 (+ (rect-width r) (rect-height r))))

(define (rect-area r)
   (* (rect-width r) (rect-height r)))