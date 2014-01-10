(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))
			
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		(list)
		(cons (accumulate   op init (map (lambda (x) (car x)) seqs))
			  (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))
			
(define (dot-product v w)
	(accumulate + 0 (map * v w)))
	
(define (matrix-*-vector m v)
	(map (lambda (x) (dot-product x v)) m))
	
(define (transpose matrix)
	(accumulate-n (lambda (x y) (cons x y)) (list) matrix))
	
(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map (lambda (v) (matrix-*-vector cols v)) m)))