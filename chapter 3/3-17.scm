(define (count-pairs x)
	(count-pairs-2 x (list)))
	
(define (count-pairs-2 x already-access)
	(define (count-pairs-iter pair)
		(cond ((not (pair? pair)) 0)
			  ((my-member? pair already-access) 0)
			  (else
				(begin (set! already-access (append already-access (list pair)))
					   (+ (count-pairs-iter (car pair))
					      (count-pairs-iter (cdr pair))
					      1)))))
	(count-pairs-iter x))
	
(define (my-member? ele set)
	(if (null? set)
		#f
		(if (eq? ele (car set))
			#t
			(my-member? ele (cdr set)))))
	
(define x (cons (cons 1 2) (cons 3 4)))
(count-pairs x)

(define t1 (cons 5 6))
(define y (list t1 t1))
(count-pairs y)

(define t2 (list 7 8))
(define z (cons t2 t2))
(count-pairs z)

(define t3 (cons 9 0))
(define t4 (cons t3 t3))
(define a (cons t4 t4))
(count-pairs a)