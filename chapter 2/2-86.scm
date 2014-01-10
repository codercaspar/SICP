;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (raise-value z) (apply-generic 'raise z))
(define (project-value z) (apply-generic 'project z))
(define (equal-value? z1 z2) (apply-generic 'equal? z1 z2))

(define (square-value z) (apply-generic 'square z))
(define (sqrt-value z) (drop-value (make-real (apply-generic 'sqrt z))))
(define (atan-value z1 z2) (drop-value (make-real (apply-generic 'atan z1 z2))))
(define (sin-value z) (drop-value (make-real (apply-generic 'sin z))))
(define (cos-value z) (drop-value (make-real (apply-generic 'cos z))))

(define (drop-value z)
	(if (equal? (type-tag z) 'scheme-number)
		z
		(let ((low (project-value z)))
			(let ((high (raise-value low)))
				(if (equal-value? z high)
					(drop-value low)
					z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-real r) ((get 'make 'real) r))
(define (make-from-real-imag r i) ((get 'make-from-real-imag 'complex) r i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add v1 v2) (apply-generic 'add v1 v2))
(define (sub v1 v2) (apply-generic 'sub v1 v2))
(define (mul v1 v2) (apply-generic 'mul v1 v2))
(define (div v1 v2) (apply-generic 'div v1 v2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
			    (let ((result (apply proc (map contents args))))
					(if (pair? result)
						(if (not (member op '(add sub mul div)))
							result
							(drop-value result))
						result))
				(cond ((or (> (length args) 2) (= (length args) 1))
						(error "No method for these types - APPLY-GENERIC" (list op type-tags)))
					  (else (let ((value1 (car args))
								  (value2 (cadr args)))
								(let ((compare (get 'compare type-tags)))
									(cond ((equal? compare 'high)
										    (apply-generic op value1 (raise-value value2)))
										  ((equal? compare 'low)
											(apply-generic op (raise-value value1) value2))
										  (else 
											(error "No method for these types compare" type-tags)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(define (install-generic-package)
	;; imported procedures
	(install-scheme-number-package)
	(install-rational-package)
	(install-real-packet)
	(install-complex-package)
	(install-hierarchies-package)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(put 'raise '(scheme-number)
		(lambda (x)  ((get 'make 'rational) x 1)))
	(put 'raise '(rational)
		(lambda (x)  ((get 'make 'real) (/ (* ((get 'numer 'rational) x) 1.0)
										   ((get 'denom 'rational) x)))))
	(put 'raise '(real)
		(lambda (x)  ((get 'make-from-real-imag 'complex) ((get 'make 'real) x) 
														  ((get 'make 'real) 0))))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(put 'project '(complex)
		(lambda (x)  ((get 'real 'complex) x)))
	(put 'project '(real)
		(lambda (x)  ((get 'make 'scheme-number) (round x))))
	(put 'project '(rational)
		(lambda (x)  ((get 'make 'scheme-number) (round (/ ((get 'numer 'rational) x) ((get 'denom 'rational) x))))))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	'done)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-hierarchies-package)
	(define (install hierarchies)
		(cond ((null? hierarchies) 'ok)
			  (else (let ((high (car hierarchies))
						  (rest (cdr hierarchies)))
						(begin
							(map (lambda (low) 
									(begin (put 'compare (list high low) 'high) 
										   (put 'compare (list low high) 'low)))
								 rest)
							(install rest))))))
	(install '(complex real rational scheme-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-scheme-number-package)
	(define (tag x) (attach-tag 'scheme-number x))
	(put 'add '(scheme-number scheme-number)
		(lambda (x y) (tag (+ x y))))
	(put 'sub '(scheme-number scheme-number)
		(lambda (x y) (tag (- x y))))
	(put 'mul '(scheme-number scheme-number)
		(lambda (x y) (tag (* x y))))
	(put 'div '(scheme-number scheme-number)
		(lambda (x y) (tag (/ x y))))
	(put 'square '(scheme-number)
		(lambda (x) (tag (* x x))))
	(put 'sqrt '(scheme-number)
		(lambda (x) (sqrt x)))
	(put 'atan '(scheme-number scheme-number)
		(lambda (x y) (atan x y)))
	(put 'sin '(scheme-number)
		(lambda (x) (sin x)))
	(put 'cos '(scheme-number)
		(lambda (x) (cos x)))
	(put 'make 'scheme-number
		(lambda (x) (tag x)))
	(put 'equal? '(scheme-number scheme-number) 
		(lambda (x y) (eq? x y)))
	'done)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rational-package)
	;; internal procedures
	(define (numer x) (car x))
	(define (denom x) (cdr x))
	(define (make-rat n d)
		(let ((g (gcd n d)))
			(cons (/ n g) (/ d g))))
	(define (add-rat x y)
		(make-rat (+ (* (numer x) (denom y))
					 (* (numer y) (denom x)))
				  (* (denom x) (denom y))))
	(define (sub-rat x y)
		(make-rat (- (* (numer x) (denom y))
				     (* (numer y) (denom x)))
				  (* (denom x) (denom y))))
	(define (mul-rat x y)
		(make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
	(define (div-rat x y)
		(make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
	;; interface to rest of the system
	(define (tag x) (attach-tag 'rational x))
	(put 'add '(rational rational)
		(lambda (x y) (tag (add-rat x y))))
	(put 'sub '(rational rational)
		(lambda (x y) (tag (sub-rat x y))))
	(put 'mul '(rational rational)
		(lambda (x y) (tag (mul-rat x y))))
	(put 'div '(rational rational)
		(lambda (x y) (tag (div-rat x y))))
	(put 'square '(rational)
		(lambda (x) (tag (mul-rat x x))))
	(put 'sqrt '(rational)
		(lambda (x) (sqrt (/ (numer x) (denom x)))))
	(put 'atan '(rational rational)
		(lambda (x y) (atan (numer (div-rat x y)) (denom (div-rat x y)))))
	(put 'sin '(rational)
		(lambda (x) (sin (/ (numer x) (denom x)))))
	(put 'cos '(rational)
		(lambda (x) (cos (/ (numer x) (denom x)))))
	(put 'numer 'rational
		(lambda (x) (numer x)))
	(put 'denom 'rational
		(lambda (x) (denom x)))
	(put 'make 'rational
		(lambda (n d) (tag (make-rat n d))))
	(put 'equal? '(rational rational) 
		(lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
	'done)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-real-packet)
	(define (tag x) (attach-tag 'real x))
	(put 'add '(real real)
		(lambda (x y) (tag (+ x y))))
	(put 'sub '(real real)
		(lambda (x y) (tag (- x y))))
	(put 'mul '(real real)
		(lambda (x y) (tag (* x y))))
	(put 'div '(real real)
		(lambda (x y) (tag (/ x y))))
	(put 'square '(real)
		(lambda (x) (tag (* x x))))
	(put 'sqrt '(real)
		(lambda (x) (sqrt x)))
	(put 'atan '(real real)
		(lambda (x y) (atan x y)))
	(put 'sin '(real)
		(lambda (x) (sin x)))
	(put 'cos '(real)
		(lambda (x) (cos x)))
	(put 'make 'real
		(lambda (x) (tag x)))
	(put 'equal? '(real real) 
		(lambda (x y) (= x y)))
	'done)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rectangular-package)
	;; internal procedures
	(define (real-part z) (car z))
	(define (imag-part z) (cadr z))
	(define (make-from-real-imag x y) (list x y))
	(define (magnitude z)
		(sqrt-value (add (square-value (real-part z))
				         (square-value (imag-part z)))))
	(define (angle z)
		(atan-value (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a)
		(list (mul r (cos-value a)) (mul r (sin-value a))))
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'rectangular x))
	(put 'real-part '(rectangular) real-part)
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(put 'make-from-real-imag 'rectangular
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'rectangular
		(lambda (r a) (tag (make-from-mag-ang r a))))
	'done)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(define (install-polar-package)
	;; internal procedures
	(define (magnitude z) (car z))
	(define (angle z) (cadr z))
	(define (make-from-mag-ang r a) (list r a))
	(define (real-part z)
		(mul (magnitude z) (cos-value (angle z))))
	(define (imag-part z)
		(mul (magnitude z) (sin-value (angle z))))
	(define (make-from-real-imag x y)
		(list (sqrt-value (add (square-value x) (square-value y)))
			  (atan-value y x)))
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'polar x))
	(put 'real-part '(polar) real-part)
	(put 'imag-part '(polar) imag-part)
	(put 'magnitude '(polar) magnitude)
	(put 'angle '(polar) angle)
	(put 'make-from-real-imag 'polar
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'polar
		(lambda (r a) (tag (make-from-mag-ang r a))))
	'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-complex-package)
    (install-polar-package)
	(install-rectangular-package)
	;; imported procedures from rectangular and polar packages
	(define (make-from-real-imag x y)
		((get 'make-from-real-imag 'rectangular) x y))
	(define (make-from-mag-ang r a)
		((get 'make-from-mag-ang 'polar) r a))
	;; internal procedures
	(define (real-part z) (apply-generic 'real-part z))
	(define (imag-part z) (apply-generic 'imag-part z))
	(define (magnitude z) (apply-generic 'magnitude z))
	(define (angle z) (apply-generic 'angle z))
	(define (add-complex z1 z2)
		(make-from-real-imag (add (real-part z1) (real-part z2))
							 (add (imag-part z1) (imag-part z2))))
	(define (sub-complex z1 z2)
		(make-from-real-imag (sub (real-part z1) (real-part z2))
							 (sub (imag-part z1) (imag-part z2))))
	(define (mul-complex z1 z2)
		(make-from-mag-ang (mul (magnitude z1) (magnitude z2))
						   (add (angle z1) (angle z2))))
	(define (div-complex z1 z2)
		(make-from-mag-ang (div (magnitude z1) (magnitude z2))
						   (sub (angle z1) (angle z2))))
	;; interface to rest of the system
	(define (tag z) (attach-tag 'complex z))
	(put 'add '(complex complex)
		(lambda (z1 z2) (tag (add-complex z1 z2))))
	(put 'sub '(complex complex)
		(lambda (z1 z2) (tag (sub-complex z1 z2))))
	(put 'mul '(complex complex)
		(lambda (z1 z2) (tag (mul-complex z1 z2))))
	(put 'div '(complex complex)
		(lambda (z1 z2) (tag (div-complex z1 z2))))
	(put 'make-from-real-imag 'complex
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'complex
		(lambda (r a) (tag (make-from-mag-ang r a))))
	(put 'real 'complex
		(lambda (z) (real-part z)))
	(put 'imag 'complex
		(lambda (z) (imag-part z)))
	(put 'equal? '(complex complex) 
		(lambda (x y) (and (equal-value? (real-part x) (real-part y)) 
						   (equal-value? (imag-part x) (imag-part y)))))
	'done)
				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (attach-tag type-tag contents)
	(list type-tag contents))
	
(define (type-tag datum)
	(if (pair? datum)
		(car datum)
		(error "Bad tagged datum - TYPE-TAG" datum)))
		
(define (contents datum)
	(if (pair? datum)
		(cadr datum)
		(error "Bad tagged datum - CONTENTS" datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-generic-package)

(equal-value? (make-scheme-number 4) (add (make-scheme-number 2) (make-scheme-number 2)))
(equal-value? (make-rational 1 1) (add (make-rational 1 2) (make-rational 1 2)))
(equal-value? (make-real 0.579) (add (make-real 0.123) (make-real 0.456)))
(equal-value? (make-from-real-imag (make-scheme-number 5) (make-scheme-number 5)) 
			  (add (make-from-real-imag (make-scheme-number 2)
										(make-scheme-number 3)) 
				   (make-from-real-imag (make-scheme-number 3)
										(make-scheme-number 2))))
(equal-value? (make-rational 7 3) (add (make-scheme-number 2) (make-rational 1 3)))
(equal-value? (make-real 2.83333333333333333333) (add (make-real 2.5) (make-rational 1 3)))
(equal-value? (make-from-real-imag (make-real 2.33333333333333333333) (make-real 3)) 
			  (add (make-from-real-imag (make-scheme-number 2) (make-scheme-number 3)) 
				   (make-rational 1 3)))
(equal-value? (make-from-real-imag (make-scheme-number 11) (make-scheme-number 3)) 
			  (add (make-from-real-imag (make-scheme-number 2)
										(make-scheme-number 3)) 
				   (make-scheme-number 9)))

(project-value (make-from-real-imag (make-scheme-number 2) (make-scheme-number 3)))
(project-value (make-from-real-imag (make-real 2.34) (make-scheme-number 3)))

(drop-value (make-from-real-imag (make-scheme-number 2) (make-scheme-number 0)))
(add (make-rational 1 3) (make-rational 2 3))
;(equal-value? (drop-value (add (make-rational 1 3) (make-rational 2 3))) (make-scheme-number 1))

(sub (make-from-real-imag (make-scheme-number 2) (make-scheme-number 8)) 
	 (make-from-real-imag (make-scheme-number 2) (make-scheme-number 3)))
(sub (make-from-real-imag (make-scheme-number 8) (make-scheme-number 3)) 
	 (make-from-real-imag (make-scheme-number 2) (make-scheme-number 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;