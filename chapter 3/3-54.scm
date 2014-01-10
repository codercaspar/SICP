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
		
(define (stream-for-each proc s)
	(if (stream-null? s)
		'done
		(begin (proc (stream-car s)) 
			   (stream-for-each proc (stream-cdr s)))))
	
(define (stream-filter pred stream)
	(cond ((stream-null? stream) the-empty-stream)
		  ((pred (stream-car stream))
			(cons-stream (stream-car stream)
						 (stream-filter pred (stream-cdr stream))))
		  (else (stream-filter pred (stream-cdr stream)))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define ones
	(cons-stream 1 ones))

(define integers
	(cons-stream 1 (add-streams ones integers)))

(define factorials 
	(cons-stream 1 (mul-streams integers factorials)))