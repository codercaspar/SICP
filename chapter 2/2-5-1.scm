(define (my-cons a b)
	(* 	(expt 2 a)
		(expt 3 b)))
		
(define (div-time n d)
	(define (iter z time)
		(if (= 0 (remainder z d))
			(iter (/ z d) (+ time 1))
			time))
	(iter n 0))
		
(define (my-car z)
	(div-time z 2))
	
(define (my-cdr z)
	(div-time z 3))