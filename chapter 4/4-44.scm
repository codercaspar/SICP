(define (multiple-dwelling)
	(let ((one   (amb (list 1 1) (list 1 2) (list 1 3) (list 1 4) (list 1 5) (list 1 6) (list 1 7) (list 1 8)))
		  (two   (amb (list 2 1) (list 2 2) (list 2 3) (list 2 4) (list 2 5) (list 2 6) (list 2 7) (list 2 8))))
	    (require (no-diagonal? two (list one)))
	    (require (distinct? (cadr two) (list (cadr one))))
	    (let ((three (amb (list 3 1) (list 3 2) (list 3 3) (list 3 4) (list 3 5) (list 3 6) (list 3 7) (list 3 8))))
	    	(require (no-diagonal? three (list two one)))
	    	(require (distinct? (cadr three) (list (cadr two) (cadr one))))
	    	(let ((four  (amb (list 4 1) (list 4 2) (list 4 3) (list 4 4) (list 4 5) (list 4 6) (list 4 7) (list 4 8))))
	    		(require (no-diagonal? four (list three two one)))
	    	    (require (distinct? (cadr four) (list (cadr three) (cadr two) (cadr one))))
	    	    (let ((five  (amb (list 5 1) (list 5 2) (list 5 3) (list 5 4) (list 5 5) (list 5 6) (list 5 7) (list 5 8))))
	    	        (require (no-diagonal? five (list four three two one)))
	    	        (require (distinct? (cadr five) (list (cadr four) (cadr three) (cadr two) (cadr one))))
	    	        (let ((six   (amb (list 6 1) (list 6 2) (list 6 3) (list 6 4) (list 6 5) (list 6 6) (list 6 7) (list 6 8))))
	    	        	(require (no-diagonal? six (list five four three two one)))
	    	            (require (distinct? (cadr six) (list (cadr five) (cadr four) (cadr three) (cadr two) (cadr one))))
	    	            (let ((seven (amb (list 7 1) (list 7 2) (list 7 3) (list 7 4) (list 7 5) (list 7 6) (list 7 7) (list 7 8))))
	    	            	(require (no-diagonal? seven (list six five four three two one)))
	    	                (require (distinct? (cadr seven) (list (cadr six) (cadr five) (cadr four) (cadr three) (cadr two) (cadr one))))
	    	                (let ((eight (amb (list 8 1) (list 8 2) (list 8 3) (list 8 4) (list 8 5) (list 8 6) (list 8 7) (list 8 8))))
	    	                	(require (no-diagonal? eight (list seven six five four three two one)))
	    	                    (require (distinct? (cadr eight) (list (cadr seven) (cadr six) (cadr five) (cadr four) (cadr three) (cadr two) (cadr one))))
	    	                    (list 
							    	(list 'one   one)
							    	(list 'two   two)
							    	(list 'three three)
							    	(list 'four  four)
							    	(list 'five  five)
							    	(list 'six   six)
							    	(list 'seven seven)
							    	(list 'eight eight))))))))))

(define (require p)
	(if (not p) (amb)))

(define (no-diagonal? head rest)
	(if (null? rest)
		true
		(if (= (abs (- (car head) (car (car rest))))
		       (abs (- (cadr head) (cadr (car rest)))))
		    false
		    (no-diagonal? head (cdr rest)))))

(define (distinct? head rest)
	(if (null? rest)
		true
		(if (= head (car rest))
		    false
		    (distinct? head (cdr rest)))))

(multiple-dwelling)