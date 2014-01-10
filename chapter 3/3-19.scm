(define (circle? input-list)
	(define (one-step p)
		(if (null? p)
			"end"
			(cdr p)))
	(define (two-step p)
		(if (null? p)
			"end"
			(if (eq? (one-step p) "end")
				"end"
				(one-step (one-step p)))))
	(define (circle-iter? one two)
		(if (or (eq? one "end") (eq? two "end"))
			#f
			(if (eq? one two)
				#t
				(circle-iter? (one-step one) (two-step two)))))
	(circle-iter? (one-step input-list) (two-step input-list)))
	
(define (make-cycle x)
	(set-cdr! (last-pair x) x)
	x)
	
(define (last-pair x)
	(if (null? (cdr x))
		x
		(last-pair (cdr x))))
		
(define z (make-cycle (list 'a 'b 'c)))

(circle? z)
(circle? (list 1 2))
(circle? (list 1))