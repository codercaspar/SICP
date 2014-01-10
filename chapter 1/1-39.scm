(define (cont-frac n d k)
	(define (iter i acc)
		(if (= i 0)
			acc
			(iter (- i 1) (/ (n i) (+ acc (d i))))))
	(iter k 0))

(define (tan-cf x k)
	(define (n i)
		(if (= 1 i)
			x
			(- (* x x))))
	(define (d i) (- (* 2 i) 1))
	(cont-frac n d k))