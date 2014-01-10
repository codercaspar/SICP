(define (display-line x)
	(newline)
	(display x))
	
(define (display-stream s)
	(stream-for-each display-line s))
	
(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))
		
(define (stream-map proc s)
	(if (stream-null? s)
		the-empty-stream
		(cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))
		
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
		
(define sum 0)

(define (accum x)
	(set! sum (+ x sum))
	sum)
	
(define seq
	(stream-map accum (stream-enumerate-interval 1 20)))
	
; (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)

(display sum)
(newline)
;这步执行完了之后sum应该是1
	
(define y (stream-filter even? seq))

(display sum)
(newline)
;这步执行完了之后sum应该是6

(define z (stream-filter
	(lambda (x) (= (remainder x 5) 0)) seq))

(display sum)
(newline)
;这步执行完了之后sum应该是10

(stream-ref y 7)
(display sum)
(newline)
;这步执行完了之后sum应该是136

(display-stream z)
(display sum)
(newline)
;这步执行完了之后sum应该是210
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;