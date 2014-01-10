(define (queens board-size)
	(queue-cols board-size board-size))
	
(define empty-board (list))
	
(define (queue-cols k board-size)
		(if (= k 0)
			(list empty-board)
			(filter (lambda (positions) (safe? k positions))
					(flatmap (lambda (rest) 
								(map (lambda (new-row)
									 (adjoin-position new-row k rest))
									 (enumerate-interval 1 board-size)))
							 (queue-cols (- k 1) board-size)))))
	
(define (adjoin-position row col other)
	(append (list (list row col)) other))
	
(define (safe? k positions)
	(let ((new-add (car positions))
		  (other (cdr positions)))
		(and (no-same-row? new-add other) (no-diagonal? new-add other))))
		
(define (no-same-row? new-add other)
	(if (null? (filter (lambda (x) (= (car new-add) (car x))) other))
		#t
		#f))
		
(define (no-diagonal? new-add other)
	(if (null? (filter (lambda (x) (= (abs (- (car new-add) (car x)))
									  (abs (- (car (cdr new-add)) (car (cdr x)))))) 
						other))
		#t
		#f))
			
(define (filter proc sequence)
	(cond ((null? sequence) (list))
		  ((proc (car sequence)) (append (list (car sequence)) (filter proc (cdr sequence))))
		  (else (filter proc (cdr sequence)))))
		  
(define (flatmap proc seq)
	(accumulate append (list) (map proc seq)))
	
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))
			
(define (enumerate-interval low high)
	(if (> low high)
		(list)
		(cons low (enumerate-interval (+ low 1) high))))