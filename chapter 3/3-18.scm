(define (circle? input-list)
	(define (circle-iter? input-list acc)
		(if (null? input-list)
			#f
			(if (my-member? (car input-list) acc)
				#t
				(circle-iter? (cdr input-list) (append acc (list (car input-list)))))))
	(circle-iter? input-list (list)))
	
(define (my-member? ele set)
	(if (null? set)
		#f
		(if (eq? ele (car set))
			#t
			(my-member? ele (cdr set)))))
			
(define (make-cycle x)
	(set-cdr! (last-pair x) x)
	x)
	
(define (last-pair x)
	(if (null? (cdr x))
		x
		(last-pair (cdr x))))
		
(define z (make-cycle (list 'a 'b 'c)))

(circle? z)