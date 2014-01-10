((unless? exp)
    (my-eval (unless->if exp) env))

(define (unless? exp) 
	(tagged-list? exp 'unless))

(define (unless-predicate exp) 
	(cadr exp))

(define (unless-consequent exp)
  	(if (not (null (cdddr exp)))
  		(cadddr exp)
  		'false))

(define (unless-alternative exp) 
	(caddr exp))

(define (unless->if exp)
	(make-if 
		(unless-predicate exp)
		(unless-consequent exp)
		(unless-alternative exp)))