#lang racket
(define (match-condition? item)
	(let ((baker    (car     item))
		  (cooper   (cadr    item))
		  (fletcher (caddr   item))
		  (miller   (cadddr  item))
		  (smith    (cddddr  item)))
	    (and
	    	(distinct? (list baker cooper fletcher miller smith))
	    	(not (= baker 5))
	    	(not (= cooper 1))
	    	(not (= fletcher 5))
	    	(not (= fletcher 1))
	    	(> miller cooper)
	    	(not (= (abs (- smith fletcher)) 1))
	    	(not (= (abs (- fletcher cooper)) 1)))))

(define (distinct? items)
	(cond ((null? items) true)
		  ((null? (cdr items)) true)
		  ((member (car items) (cdr items)) false)
		  (else (distinct? (cdr items)))))

(define (list-all-possible n)
	(if (= n 1)
		(list 1 2 3 4 5)
		(apply append
			(map 
			    (lambda (x) 
				    (map 
					    (lambda (y) (cons y x))
					    (list 1 2 3 4 5)))
			    (list-all-possible (- n 1))))))

(filter	 (lambda (x) (match-condition? x)) (list-all-possible 5))