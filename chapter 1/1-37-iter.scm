(define (cont-frac n d k)
	(define (iter n d i acc)
		(if (= i 0)
			acc
			(iter n d (- i 1) (/ (n i) (+ acc (d i))))))
	(iter n d k 0))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 4)