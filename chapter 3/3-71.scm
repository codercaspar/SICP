;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (weighted-pairs s t weight)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(merge-weighted
			(stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
			(weighted-pairs (stream-cdr s) (stream-cdr t) weight) 
			weight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-weighted s1 s2 weight)
  	(cond 	((stream-null? s1) s2)
          	((stream-null? s2) s1)
        	(else
        		(let ((s1car (stream-car s1))
               		  (s2car (stream-car s2)))
           				(if (<= (weight s1car) (weight s2car))
               				(cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
               				(cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))

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

(define (stream-filter pred stream)
	(cond ((stream-null? stream) the-empty-stream)
		  ((pred (stream-car stream))
			(cons-stream (stream-car stream)
						 (stream-filter pred (stream-cdr stream))))
		  (else (stream-filter pred (stream-cdr stream)))))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			
(define (add-streams s1 s2) 
	(stream-map + s1 s2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

(define (stream-show s time)
	(if (= time 0)
		'done
		(begin (display (stream-car s))
			   (newline)
			   (stream-show (stream-cdr s) (- time 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (weight x)
	(apply sum-cube x))

(define (sum-cube x y)
	(+ (* x x x) (* y y y)))
	
(define test
	(weighted-pairs integers integers weight))
		
(define (find-ram-num s)
	(let ((x (stream-car s))
	      (y (stream-car (stream-cdr s))))
		(if (equal? (weight x) (weight y))
			(cons-stream (weight x) (find-ram-num (stream-cdr (stream-cdr s))))
			(find-ram-num (stream-cdr s)))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;