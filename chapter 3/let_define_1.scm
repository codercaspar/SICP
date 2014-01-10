;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sqrt-improve guess x)
	(average guess (/ x guess)))
	
(define (average a1 a2)
	(/ (+ a1 a2) 2.0))
	
(define (sqrt-stream x)
	(let ((guesses (cons-stream 1.0 
								(stream-map (lambda (guess) (sqrt-improve guess x)) guesses))))
		guesses))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
