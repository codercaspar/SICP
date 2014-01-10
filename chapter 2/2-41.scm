(define (find-equal-triples n s) 
	(filter (lambda (triple) (= s (+ (car triple) (car (cdr triple)) (car (cdr (cdr triple)))))) 
			(unique-triples n)))

(define (unique-triples n)
	(flatmap
		(lambda (i) 
			(map 
				(lambda (j) (append i (list j)))
				(enumerate-interval 1 (- (car (cdr i)) 1))))
		(unique-pairs n)))

(define (unique-pairs n)
   (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))
	
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
		
(define (filter proc sequence)
	(cond ((null? sequence) (list))
		  ((proc (car sequence)) (append (list (car sequence)) (filter proc (cdr sequence))))
		  (else (filter proc (cdr sequence)))))