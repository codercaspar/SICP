(define ones
	(cons-stream 1 ones))

(define integers
	(cons-stream 1 (add-streams ones integers)))
	
(define (add-streams s1 s2)
	(stream-map + s1 s2))
	
(define (partial-sums s)
	(cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))
	
(define test (partial-sums integers))

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

(stream-ref test 1)
(stream-ref test 2)
(stream-ref test 3)
(stream-ref test 4)
(stream-ref test 5)