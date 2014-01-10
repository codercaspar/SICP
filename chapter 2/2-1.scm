(define (make-rat n d)
	(define (make-rat-normalize x y)
		(let ((g (gcd (abs x) (abs y))))
			(cons (/ x g) (/ y g))))
	(define (same-sign? x y)
		(> (* x y) 0))
	(cond ((same-sign? n d) (make-rat-normalize (abs n) (abs d)))
		  (else (make-rat-normalize (- 0 (abs n)) (abs d)))))

(define (number x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
	(newline)
	(display (number x))
	(display "/")
	(display (denom x)))

(define (add-rat x y)
	(make-rat (+ (* (number x) (denom y))
				 (* (number y) (denom x)))
			  (* (denom x) (denom y))))
			  
(define (sub-rat x y)
	(make-rat (- (* (number x) (denom y))
				 (* (number y) (denom x)))
			  (* (denom x) (denom y))))
			  
(define (mul-rat x y)
	(make-rat (* (number x) (number y))
			  (* (denom x) (denom y))))
			  
(define (div-rat x y)
	(make-rat (* (number x) (denom y))
			  (* (denom x) (number y))))
			  
(define (equal-rat? x y)
	(= (* (number x) (denom y))
	   (* (denom x) (number y))))