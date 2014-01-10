;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge s1 s2)
	(cond ((stream-null? s1) s2)
		  ((stream-null? s2) s1)
		  (else
			(let ((s1car (stream-car s1))
				  (s2car (stream-car s2)))
				(cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
				      ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
					  (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))
					  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define S (cons-stream 1 (merge  (merge (scale-stream S 2) (scale-stream S 3))
								 (scale-stream S 5))))
								 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(stream-ref S 0)								 
(stream-ref S 1)
(stream-ref S 2)
(stream-ref S 3)
(stream-ref S 4)
(stream-ref S 5)