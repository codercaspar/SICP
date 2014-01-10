(define (count-pairs x)
	(if (not (pair? x))
		0
		(+ 	(count-pairs (car x))
			(count-pairs (cdr x))
			1)))

;3
(define x (cons (cons 'a 'b) (cons 'a 'b)))
(count-pairs x)
;4
(define t1 (cons 'a 'b))
(define y (list t1 t1))
(count-pairs y)
;5
(define t2 (list 'a 'b))
(define z (cons t2 t2))
(count-pairs z)
;7
(define t3 (cons 'a 'b))
(define t4 (cons t3 t3))
(define a (cons t4 t4))
(count-pairs a)
;iter-forever
(define t5 (cons 'a 'b))
(define t6 (cons 'c 'd))
(define b (cons t5 t6))
(set-cdr! t6 b)
(count-pairs b)