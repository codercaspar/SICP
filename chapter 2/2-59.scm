(define (union-set set1 set2)
	(cond ((null? set2) set1)
	      ((null? set1) set2)
		  ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
		  (else (cons (car set1) (union-set (cdr set1) set2)))))
		
(define (element-of-set? x set)
	(cond ((null? set) #f)
		  ((equal? x (car set)) #t)
		  (else (element-of-set? x (cdr set)))))
		  
(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)))