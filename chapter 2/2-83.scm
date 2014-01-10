;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(define (install-raise-package)
	;; imported procedures
	(install-scheme-number-package)
	(install-rational-package)
	(install-real-packet)
	(install-complex-package)
	'done)

(define (raise-value z) (apply-generic 'raise z))

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
	(put 'raise '(scheme-number) 
		(lambda (x)  ((get 'make 'rational) x 1)))
	(put 'make 'scheme-number
		(lambda (x) (tag x)))
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
	(put 'numer '(rational) numer)
	(put 'denom '(rational) numer)
	(put 'add '(rational rational)
		(lambda (x y) (tag (add-rat x y))))
	(put 'sub '(rational rational)
		(lambda (x y) (tag (sub-rat x y))))
	(put 'mul '(rational rational)
		(lambda (x y) (tag (mul-rat x y))))
	(put 'div '(rational rational)
		(lambda (x y) (tag (div-rat x y))))
	(put 'raise '(rational) 
		(lambda (x)  ((get 'make 'real) (/ (* (numer x) 1.0) (denom x)))))
	(put 'make 'rational
		(lambda (n d) (tag (make-rat n d))))
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
	(put 'raise '(real) 
		(lambda (x)  ((get 'make-from-real-imag 'complex) x 0)))
	(put 'make 'real
		(lambda (x) (tag x)))
	'done)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-rectangular-package)
	;; internal procedures
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (make-from-real-imag x y) (cons x y))
	(define (magnitude z)
		(sqrt (+ (square (real-part z))
				 (square (imag-part z)))))
	(define (angle z)
		(atan (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a)
		(cons (* r (cos a)) (* r (sin a))))
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
	(define (angle z) (cdr z))
	(define (make-from-mag-ang r a) (cons r a))
	(define (real-part z)
		(* (magnitude z) (cos (angle z))))
	(define (imag-part z)
		(* (magnitude z) (sin (angle z))))
	(define (make-from-real-imag x y)
		(cons (sqrt (+ (square x) (square y)))
			  (atan y x)))
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
		(make-from-real-imag (+ (real-part z1) (real-part z2))
							 (+ (imag-part z1) (imag-part z2))))
	(define (sub-complex z1 z2)
		(make-from-real-imag (- (real-part z1) (real-part z2))
							 (- (imag-part z1) (imag-part z2))))
	(define (mul-complex z1 z2)
		(make-from-mag-ang (* (magnitude z1) (magnitude z2))
						   (+ (angle z1) (angle z2))))
	(define (div-complex z1 z2)
		(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
						   (- (angle z1) (angle z2))))
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
	'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args))
				(error "No method for these types - APPLY-GENERIC" (list op type-tags))))))
				
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

(define (attach-tag type-tag contents)
	(cons type-tag contents))
	
(define (type-tag datum)
	(if (pair? datum)
		(car datum)
		(error "Bad tagged datum - TYPE-TAG" datum)))
		
(define (contents datum)
	(if (pair? datum)
		(cdr datum)
		(error "Bad tagged datum - CONTENTS" datum)))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-raise-package)
(raise-value ((get 'make 'scheme-number) 2))
(raise-value (raise-value ((get 'make 'scheme-number) 2)))
(raise-value (raise-value (raise-value ((get 'make 'scheme-number) 2))))
(raise-value (raise-value (raise-value (raise-value ((get 'make 'scheme-number) 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;