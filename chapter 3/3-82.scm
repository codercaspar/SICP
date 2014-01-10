;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define circle-center 
	(cons 100 100))
(define left-down 
	(cons 0 0))
(define right-top 
	(cons 200 200))
(define circle-r 
	100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (random range))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ones
	(cons-stream (cons 1 1) ones))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define random-numbers
	(stream-map 
		(lambda (x) (list (* (random-in-range (car left-down) (car right-top)) (car x))
						  (* (random-in-range (car left-down) (car right-top)) (cdr x))))
		ones))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x)
	(* x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (map-successive-pairs f s)
	(cons-stream 
		(apply f (stream-car s))
		(map-successive-pairs f (stream-cdr s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cesaro-stream
	(map-successive-pairs 
		(lambda (x y) 
			(<= (+ (square (- x (car circle-center))) 
			   	   (square (- y (cdr circle-center))))
				(square circle-r))) 
		random-numbers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (monte-carlo experiment-stream passed failed)
	(define (next passed failed)
		(cons-stream
			(/ passed (+ passed failed))
			(monte-carlo (stream-cdr experiment-stream) passed failed)))
	(if (stream-car experiment-stream)
		(next (+ passed 1) failed)
		(next passed (+ failed 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi
	(stream-map 
		(lambda (p) 
			(*
				(/ (* p (* (- (car right-top) (car left-down))
					   (- (cdr right-top) (cdr left-down))))
				   (square circle-r))
				1.0))
		(monte-carlo cesaro-stream 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stream-ref s n)
	(if (= n 0)
		(stream-car s)
		(stream-ref (stream-cdr s) (- n 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

(define (stream-show s time)
	(if (= time 0)
		'done
		(begin (display (stream-car s))
			   (newline)
			   (stream-show (stream-cdr s) (- time 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map (cons proc (map stream-cdr argstreams))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;