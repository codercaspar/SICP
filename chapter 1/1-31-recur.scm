(define (up-div n base)
	(cond ((= 0 (remainder n base)) (/ n base))
		  (else (+ (/ (- n (remainder n base)) base) 1))))

(define (term n) (+ (* n 2) 1))

(define (factorial term a next b)
	(if (> a b)
		1
		(* (term a) (factorial term (next a) next b))))

(define (inc n) (+ n 1))

(define (fac a) 
	(/  (+ (term (up-div a 2)) 1.0) 
		(term (up-div a 2))))

(define (product n)
	(define finish (+ (term (up-div n 2)) 1))
	(/ (* (factorial fac 1 inc n) 2) finish))