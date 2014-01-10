(define (attach-tag tag contents)
	(cond ((number? contents) contents)
		  (else (cons tag contents))))
		  
(define (type-tag datum)
	(cond ((number? datum) 'scheme-number)
	      ((pair? datum) (car datum))
		  (else "error")))
		  
(define (contents datum)
	(cond ((number? datum) datum)
	      ((pair? datum) (cdr datum))
		  (else "error")))