(define (solve dy0 y0 a b dt)
	(define y   (integral (delay dy) y0 dt))
	(define dy  (integral (delay ddy) dy0 dt))
	(define ddy (add-streams (scale-stream y b) (scale-stream dy a)))
	y)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map (cons proc (map stream-cdr argstreams))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(define (add-streams s1 s2) 
	(stream-map + s1 s2))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;