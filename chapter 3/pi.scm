(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))
		
(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map (cons proc (map stream-cdr argstreams))))))
			
(define (pi-summands n)
	(cons-stream (/ 1.0 n) (stream-map - (pi-summands (+ n 2)))))
	
(define pi-stream
	(scale-stream (partial-sums (pi-summands 1)) 4))
	
(define (partial-sums s)
	(cons-stream 0 (add-streams s (partial-sums s))))
	
(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream))
	
(define (euler-transform s)
	(let ((s0 (stream-ref s 0)) ; Sn?1
		  (s1 (stream-ref s 1)) ; Sn
		  (s2 (stream-ref s 2))) ; Sn+1
		(cons-stream (- s2 (/ (square (- s2 s1)) (+ s0 (* -2 s1) s2)))
					 (euler-transform (stream-cdr s)))))
					 
(define (make-tableau transform s)
	(cons-stream s (make-tableau transform (transform s))))
	
(define (accelerated-sequence transform s)
	(stream-map stream-car (make-tableau transform s)))
	
(stream-ref (accelerated-sequence euler-transform pi-stream) 10)