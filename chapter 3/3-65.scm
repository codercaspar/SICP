;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map (cons proc (map stream-cdr argstreams))))))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-tableau transform s)
	(cons-stream s (make-tableau transform (transform s))))
	
(define (accelerated-sequence transform s)
	(stream-map stream-car (make-tableau transform s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (euler-transform s)
	(let ((s0 (stream-ref s 0))  ; Sn-1
		  (s1 (stream-ref s 1))  ; Sn
		  (s2 (stream-ref s 2))) ; Sn+1
		(cons-stream (- s2 (/ (square (- s2 s1)) (+ s0 (* -2 s1) s2)))
					 (euler-transform (stream-cdr s)))))
					 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					 
(define (partial-sums s)
	(cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))
					 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lg2-summands n)
	(cons-stream (/ 1.0 n)
	(stream-map - (lg2-summands (+ n 1)))))
	
(define lg2-stream
	(partial-sums (lg2-summands 1)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test
	(accelerated-sequence euler-transform lg2-stream))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;