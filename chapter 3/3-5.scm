(define (random-in-range low high)
	(let ((range (- high low)))
		(+ low (random range))))
		
(define (square x)
	(* x x))
		
(define circle-center (list 1 1))
(define left-down (list 0 0))
(define right-top (list 2 2))

(define (estimate-pi trials)
	(* 4 (monte-carlo trials test)))
	
(define (test)
	(let ((x (random-in-range (car left-down) (car right-top)))
		  (y (random-in-range (cadr left-down) (cadr right-top))))
		(<= (+ (square (- x (car circle-center))) 
			   (square (- y (cadr circle-center)))) 
			1)))

(define (monte-carlo trials experiment)
	(define (iter trials-remaining trials-passed)
		(cond ((= trials-remaining 0)  (/ trials-passed trials))
			  ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
			  (else (iter (- trials-remaining 1) trials-passed))))
	(iter trials 0))
	
(estimate-pi 10000)
(estimate-pi 100000)
(estimate-pi 1000000)
(estimate-pi 9000000)