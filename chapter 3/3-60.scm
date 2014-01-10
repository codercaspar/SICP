;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mul-series s1 s2)
	(cons-stream (* (stream-car s1) (stream-car s2))
				 (add-streams  (scale-stream (stream-cdr s2) (stream-car s1))
							   (mul-series (stream-cdr s1) s2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-line x)
	(newline)
	(display x))

(define (stream-for-each proc s)
	(if (stream-null? s)
		'done
		(begin (proc (stream-car s)) 
			   (stream-for-each proc (stream-cdr s)))))

(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))
		
(define (scale-stream stream factor)
	(stream-map (lambda (x) (* x factor)) stream))
		
(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map (cons proc (map stream-cdr argstreams))))))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (constant num)
	(cons-stream num (constant num)))

(define ones 
	(cons-stream 1 ones))
	
(define zeros 
	(cons-stream 0 zeros))
	
(define (add-streams s1 s2) 
	(stream-map + s1 s2))
	
(define integers
	(cons-stream 1 (add-streams ones integers)))

(define (integrate-series input-stream)
	(stream-map / input-stream integers))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define exp-series
	(cons-stream 1 (integrate-series exp-series)))
	
(define cosine-series
	(cons-stream 1 (stream-map - zeros (integrate-series sine-series))))

(define sine-series
	(cons-stream 0 (integrate-series cosine-series)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define power-sine-series
	(mul-series sine-series sine-series))
	
(define power-cosine-series
	(mul-series cosine-series cosine-series))
	
(define test
	(add-streams power-sine-series power-cosine-series))
	
(stream-for-each display-line test)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;