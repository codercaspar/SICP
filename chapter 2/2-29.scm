(define (make-mobile left right)
	(list left right))
	
(define (mobile-left mobile)
	(car mobile))
	
(define (mobile-right mobile)
	(car (cdr mobile)))
	
	
(define (make-branch len structure)
	(list len structure))
	
(define (branch-length branch)
	(car branch))
	
(define (branch-structure branch)
	(car (cdr branch)))
	
(define (branch-weight branch)
		(if (not (pair? (branch-structure branch)))
			(branch-structure branch)
			(total-weight (branch-structure branch))))
	
(define (total-weight mobile)
	(+ (branch-weight (mobile-left  mobile))
	   (branch-weight (mobile-right mobile))))
	   
(define (balance-torque branch)
	(* (branch-length  branch) (branch-weight  branch)))
	   
(define (mobile-balanced? mobile)
	(let ((left  (mobile-left  mobile))
		  (right (mobile-right mobile)))
		(cond ((and (pair? (branch-structure left))
					(pair? (branch-structure right)))
				(and (= (balance-torque left) (balance-torque right))
					 (mobile-balanced? left)
					 (mobile-balanced? right)))
			  ((and (not (pair? (branch-structure left)))
				    (pair? (branch-structure right)))
				(and (= (balance-torque left) (balance-torque right))
					 (mobile-balanced? right)))
			  ((and (pair? (branch-structure left))
				    (not (pair? (branch-structure right))))
				(and (= (balance-torque left) (balance-torque right))
					 (mobile-balanced? left)))
			  (else (= (balance-torque left) (balance-torque right))))))