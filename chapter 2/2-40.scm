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