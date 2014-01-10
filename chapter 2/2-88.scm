;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-polynomial-package)
	;; internal procedures
	;; representation of poly
	(define (make-poly variable term-list) (list variable term-list))
	(define (variable p) (car p))
	(define (term-list p) (cadr p))
	(define (same-variable? v1 v2)
		(eq? v1 v2))
	(define (negation p)
		(make-poly (variable p) (map (lambda (term) 
										(list (order term) (negation-value (coeff term)))) 
									 (term-list p))))
	(define (add-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
			(make-poly (variable p1)
					   (add-terms (term-list p1) (term-list p2)))
			(error "Polys not in same var - ADD-POLY" (list p1 p2))))
	(define (mul-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
			(make-poly (variable p1) 
					   (mul-terms (term-list p1) (term-list p2)))
			(error "Polys not in same var - MUL-POLY" (list p1 p2))))
	(define (add-terms L1 L2)
		(cond ((empty-termlist? L1) L2)
			  ((empty-termlist? L2) L1)
			  (else
				(let ((t1 (first-term L1))
					  (t2 (first-term L2)))
					(cond ((> (order t1) (order t2))
							(adjoin-term t1 (add-terms (rest-terms L1) L2)))					
						  ((< (order t1) (order t2))
							(adjoin-term t2 (add-terms L1 (rest-terms L2))))
						  (else
							(adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2)))
										 (add-terms (rest-terms L1) (rest-terms L2)))))))))
	(define (mul-terms L1 L2)
		(if (empty-termlist? L1)
			(the-empty-termlist)
			(add-terms (mul-term-by-all-terms (first-term L1) L2) (mul-terms (rest-terms L1) L2))))
	(define (mul-term-by-all-terms t1 L)
		(if (empty-termlist? L)
			(the-empty-termlist)
			(let ((t2 (first-term L)))
				(adjoin-term
					(make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
					(mul-term-by-all-terms t1 (rest-terms L))))))
	(define (adjoin-term term term-list)
		(if (=zero? (coeff term))
			term-list
			(cons term term-list)))
	(define (the-empty-termlist) '())
	(define (first-term term-list) (car term-list))
	(define (rest-terms term-list) (cdr term-list))
	(define (empty-termlist? term-list) (null? term-list))
	(define (make-term order coeff) (list order coeff))
	(define (order term) (car term))
	(define (coeff term) (cadr term))
	(define (=zero? term-coeff)
		(equal-zero? term-coeff))
	;; interface to rest of the system
	(define (tag p) (attach-tag 'polynomial p))
	(put 'add '(polynomial polynomial)
		(lambda (p1 p2) (tag (add-poly p1 p2))))
	(put 'sub '(polynomial polynomial)
		(lambda (p1 p2) (tag (add-poly p1 (negation p2)))))
	(put 'mul '(polynomial polynomial)
		(lambda (p1 p2) (tag (mul-poly p1 p2))))
	(put 'make 'polynomial
		(lambda (var terms) (tag (make-poly var terms))))
	(put 'equal-zero? '(polynomial) 
		(lambda (p) (if (empty-termlist? (term-list p))
						#t
						(myand (map equal-zero? (map coeff (term-list p)))))))
    (put 'negation '(polynomial)
		(lambda (p) (tag (negation p))))
	'done)
	
(define (myand l)
	(cond ((null? l) #t)
		  ((car l) (myand (cdr l)))
		  (else #f)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (raise-value z) (apply-generic 'raise z))
(define (project-value z) (apply-generic 'project z))
(define (equal-value? z1 z2) (apply-generic 'equal? z1 z2))
(define (equal-zero? z) (apply-generic 'equal-zero? z))
(define (negation-value z) (apply-generic 'negation z))

(define (square-value z) (apply-generic 'square z))
(define (sqrt-value z) (drop-value (make-real (apply-generic 'sqrt z))))
(define (atan-value z1 z2) (drop-value (make-real (apply-generic 'atan z1 z2))))
(define (sin-value z) (drop-value (make-real (apply-generic 'sin z))))
(define (cos-value z) (drop-value (make-real (apply-generic 'cos z))))

(define (drop-value z)
	(cond ((equal? (type-tag z) 'scheme-number) z)
		  ((equal? (type-tag z) 'polynomial) z)
		  (else (let ((low (project-value z)))
			(let ((high (raise-value low)))
				(if (equal-value? z high)
					(drop-value low)
					z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-real r) ((get 'make 'real) r))
(define (make-from-real-imag r i) ((get 'make-from-real-imag 'complex) r i))
(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))

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
	(install-polynomial-package)
	(install-hierarchies-package)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(put 'raise '(scheme-number)
		(lambda (x) ((get 'make 'rational) x 1)))
	(put 'raise '(rational)
		(lambda (x) ((get 'make 'real) (- (+ (/ (* ((get 'numer 'rational) x) 1.0)
										            ((get 'denom 'rational) x)) 1.0) 1.0))))
	(put 'raise '(real)
		(lambda (x) ((get 'make-from-real-imag 'complex) ((get 'make 'real) x) 
														  ((get 'make 'scheme-number) 0))))
	(put 'raise '(complex)
		(lambda (x) ((get 'make 'polynomial) 'x (list (list 0 x)))))
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
	(put 'equal-zero? '(scheme-number) 
		(lambda (x) (= x 0)))
	(put 'negation '(scheme-number)
		(lambda (x) (- 0 x)))
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
	(put 'equal-zero? '(rational) 
		(lambda (x) (= (numer x) 0)))
    (put 'negation '(rational)
		(lambda (x) (make-rat (- 0 (numer x)) (denom x))))
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
	(put 'equal-zero? '(real) 
		(lambda (x) (= x 0.0)))
	(put 'negation '(real)
		(lambda (x) (- 0 x)))
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
	(put 'equal-zero? '(complex) 
		(lambda (x) (and (equal-value? (real-part x) (make-scheme-number 0)) 
						 (equal-value? (imag-part x) (make-scheme-number 0)))))
	(put 'negation '(complex)
		(lambda (x) (make-from-real-imag (negation-value (real-part x)) (negation-value (imag-part x)))))
	'done)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (myinteger? z)
	(if (integer? z)
		(not (member #\. (string->list (number->string z))))
		#f))

(define (attach-tag tag contents)
	(cond ((myinteger? contents) 
				(if (eq? tag 'scheme-number)
					contents
					(error "Bad tag contents - TYPE-TAG" (list tag contents))))
	      ((real? contents) 
				(if (eq? tag 'real)
					contents
					(error "Bad tag contents - TYPE-TAG" (list tag contents))))
		  (else (list tag contents))))
	
(define (type-tag datum)
    (cond ((myinteger? datum) 'scheme-number)
	      ((real? datum) 'real)
	      ((pair? datum) (car datum))
		  (error "Bad tagged datum - TYPE-TAG" datum)))
		
(define (contents datum)
    (cond ((myinteger? datum) datum)
	      ((real? datum) datum)
	      ((pair? datum) (cadr datum))
		  (error "Bad tagged datum - CONTENTS" datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-generic-package)
(equal-value? 4 (add 2 2))
(equal-value? (make-rational 1 1) (add (make-rational 1 2) (make-rational 1 2)))
(equal-value? 0.579 (add 0.123 0.456))
(equal-value? (make-from-real-imag 5 5) (add (make-from-real-imag 2 3) (make-from-real-imag 3 2)))
(equal-value? (make-rational 7 3) (add 2 (make-rational 1 3)))
(equal-value? (make-from-real-imag 2.333333333333333 3) (add (make-from-real-imag 2 3) (make-rational 1 3)))
(equal-value? (make-from-real-imag 11 3) (add (make-from-real-imag 2 3) 9))

(project-value (make-from-real-imag 2 3))
(project-value (make-from-real-imag 2.34 3))
(drop-value (make-from-real-imag 2 0))
(add (make-rational 1 3) (make-rational 2 3))
(sub (make-from-real-imag 2 8) (make-from-real-imag 2 3))
(sub (make-from-real-imag 8 3) (make-from-real-imag 2 3))

(equal-zero? (make-polynomial 'x (list)))
(equal-zero? (make-polynomial 'x (list (list 1 0) (list 2 0))))
(equal-zero? (make-polynomial 'x (list (list 1 (make-polynomial 'y (list))))))
(equal-zero? (make-polynomial 'x (list (list 2 (make-polynomial 'y (list))) (list 1 (make-polynomial 'z (list))))))
(equal-zero? (make-polynomial 'x (list (list 1 (make-polynomial 'y (list (list 1 (make-polynomial 'z (list)))))))))

(add (make-polynomial 'x (list)) (make-polynomial 'x (list (list 1 2))))
(add (make-polynomial 'x (list (list 3 2) (list 1 2))) (make-polynomial 'x (list (list 1 2))))
(sub (make-polynomial 'x (list (list 3 2) (list 1 2))) (make-polynomial 'x (list (list 2 1) (list 1 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;