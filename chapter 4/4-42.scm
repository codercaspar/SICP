(define (multiple-dwelling)
	(let ((Betty (amb 1 2 3 4 5))
		  (Ethel (amb 1 2 3 4 5))
		  (Joan  (amb 1 2 3 4 5))
		  (Kitty (amb 1 2 3 4 5))
		  (Mary  (amb 1 2 3 4 5)))
	    (require (one-true-and-one-false? (= Kitty 2) (= Betty 3)))
	    (require (one-true-and-one-false? (= Ethel 1) (= Joan 2)))
	    (require (one-true-and-one-false? (= Joan 3) (= Ethel 5)))
	    (require (one-true-and-one-false? (= Kitty 2) (= Mary 4)))
	    (require (one-true-and-one-false? (= Mary 4) (= Betty 1)))
	    (require (distinct? (list Betty Ethel Joan Kitty Mary)))
	    (list 
	    	(list 'Betty Betty)
	    	(list 'Ethel Ethel)
	    	(list 'Joan  Joan)
	    	(list 'Kitty Kitty)
	    	(list 'Mary  Mary))))

(define (one-true-and-one-false? condition1 condition2)
	(if (equal? condition1 true)
		(if (equal? condition2 false)
			true
			false)
		(if (equal? condition2 true)
			true
			false)))

(define (require p)
	(if (not p) (amb)))

(define (distinct? items)
	(cond ((null? items) true)
		  ((null? (cdr items)) true)
		  ((member (car items) (cdr items)) false)
		  (else (distinct? (cdr items)))))

(multiple-dwelling)