(define (cont-frac n d k)
	(if (= k 0)
		0
		(/ (n (- k (- k 1))) (+ (d (- k (- k 1))) (cont-frac n d (- k 1))))))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 4)