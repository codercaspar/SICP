(define (display-line x)
	(newline)
	(display x))
	
(define (display-stream s)
	(stream-for-each display-line s))
	
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
		  
(define (stream-enumerate-interval low high)
	(if (> low high)
		the-empty-stream
		(cons-stream low (stream-enumerate-interval (+ low 1) high))))
		
(define (myadd . args)
	(display "once")
	(newline)
	(apply + args))
		
(define (add-streams s1 s2)
	(begin (display "once")
		   (newline)
		   (stream-map myadd s1 s2)))
		   
(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))