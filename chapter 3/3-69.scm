;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (triples a b c)
	(cons-stream 
		(list (stream-car a) (stream-car b) (stream-car c))
		(interleave
			(stream-map (lambda (x) (append (list (stream-car a)) x)) (stream-cdr (pairs b c)))
			(triples (stream-cdr a) (stream-cdr b) (stream-cdr c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pairs s t)
	(cons-stream (list (stream-car s) (stream-car t))
	(interleave
		(stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
		(pairs (stream-cdr s) (stream-cdr t)))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
(define integers
	(cons-stream 1 (add-streams ones integers)))
	
(define ones
	(cons-stream 1 ones))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map (cons proc (map stream-cdr argstreams))))))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			
(define (add-streams s1 s2) 
	(stream-map + s1 s2))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
(define (stream-filter pred stream)
	(cond ((stream-null? stream) the-empty-stream)
		  ((pred (stream-car stream))
			(cons-stream (stream-car stream)
						 (stream-filter pred (stream-cdr stream))))
		  (else (stream-filter pred (stream-cdr stream)))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interleave s1 s2)
	(if (stream-null? s1)
		s2
		(cons-stream (stream-car s1) (interleave s2 (stream-cdr s1)))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test
	(triples integers integers integers))
	
(define (sqart x)
	(* x x))
	
(define pythagorean-triples
	(stream-filter (lambda (x)
						(let ((a (car x))
							  (b (cadr x))
							  (c (caddr x)))
							(if (= (sqart c) (+ (sqart a) (sqart b)))
								#t
								#f))) 
				test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

(define (stream-show s time)
	(if (= time 0)
		'done
		(begin (display (stream-car s))
			   (newline)
			   (stream-show (stream-cdr s) (- time 1)))))
			   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;