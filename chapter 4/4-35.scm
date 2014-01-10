(define (an-integer-between low high)
	(require (<= low high))
	(amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
	(let ((i (an-integer-between low high)))
		(let ((j (an-integer-between i high)))
			(let ((k (an-integer-between j high)))
				(require (= (+ (* i i) (* j j)) (* k k)))
				(list i j k)))))

(define (require p)
	(if (not p) (amb)))

(a-pythagorean-triple-between 1 10)