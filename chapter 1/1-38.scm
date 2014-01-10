(define (cont-frac n d k)
	(define (iter i acc)
		(if (= i 0)
			acc
			(iter (- i 1) (/ (n i) (+ acc (d i))))))
	(iter k 0))

(define (euler-expansion n)
	(define (d i)
		(if (= 0 (remainder (+ i 1) 3))
			(* 2 (/ (+ i 1) 3))
			1))
	(cont-frac (lambda (i) 1.0) d n))

(define e (+ 2 (euler-expansion 10)))