(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))
			
(define (enumerate-tree tree)
   (cond ((null? tree) (list))
         ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                       (enumerate-tree (cdr tree))))))
			
(define (count-leaves tree)
	(accumulate + 0 (map (lambda (x) 1) (enumerate-tree tree))))