(define (all-possible choices)
	(if (null? choices)
		(list '())
		(apply append 
			(map 
				(lambda (x) (map 
								(lambda (y) (cons x y))
								(all-possible (rest-choices choices x))))
				choices))))

(define (rest-choices choices exclude)
	(if (null? choices)
		'()
		(if (equal? exclude (car choices))
			(cdr choices)
			(cons (car choices) (rest-choices (cdr choices) exclude)))))

(define (multiple-dwelling)
	(let ((all (all-possible (list 1 2 3 4 5))))
		(try-test all)))

(define (try-test lists)
	(if (null? lists)
		#f
		(let ((one-try (one-try-test (car lists))))
			(if one-try
				(car lists)
				(try-test (cdr lists))))))

(define (one-try-test one-possible)
	(let ((baker    (car     one-possible))
		  (cooper   (cadr    one-possible))
		  (fletcher (caddr   one-possible))
		  (miller   (cadddr  one-possible))
		  (smith    (car (cddddr one-possible))))
	    (and (not (= baker 5)) 
	    	 (not (= cooper 1)) 
	    	 (not (= fletcher 5)) 
	    	 (not (= fletcher 1)) 
	    	 (> miller cooper)
	    	 (not (= (abs (- smith fletcher)) 1))
	    	 (not (= (abs (- fletcher cooper)) 1)))))

(multiple-dwelling)