(define (make-mobile left right)
	(cons left right))
	
(define (mobile-left mobile)
	(car mobile))
	
(define (mobile-right mobile)
	(cdr mobile))
	
(define (make-branch len structure)
	(cons len structure))
	
(define (branch-length branch)
	(car branch))
	
(define (branch-structure branch)
	(cdr branch))
	
(define (branch-weight branch)
		(if (not (pair? (branch-structure branch)))
			(branch-structure branch)
			(total-weight (branch-structure branch))))
	
(define (total-weight mobile)
	(+ (branch-weight (mobile-left  mobile))
	   (branch-weight (mobile-right mobile))))
	   
(define (branch-balanced? branch)
   (if (pair? (branch-structure branch))
       (mobile-balanced? (branch-structure branch))
       (cons #t (branch-weight  branch))))
	   
(define (mobile-balanced? mobile)
	(let ((lvalue (branch-balanced? (mobile-left  mobile)))
		  (rvalue (branch-balanced? (mobile-right mobile))))
		(if (and (car lvalue) (car rvalue))
			(cons (= (* (branch-length  (mobile-left   mobile)) (cdr lvalue))
				     (* (branch-length  (mobile-right  mobile)) (cdr rvalue)))
				(+ (cdr lvalue) (cdr rvalue)))
			(cons #f (+ (cdr lvalue) (cdr rvalue))))))