;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (high? v1 v2) (get 'var-compare (list v1 v2)))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-polynomials-hierarchies-package)
	(define (install hierarchies)
		(cond ((null? hierarchies) 'ok)
			  (else (let ((low (car hierarchies))
						  (rest (cdr hierarchies)))
						(begin
							(map (lambda (high) 
									(begin (put 'var-compare (list high low) #t) 
										   (put 'var-compare (list low high) #f)))
								 rest)
							(install rest))))))
	(install '(x y z)))
	
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
	(install '(polynomial complex real rational scheme-number)))

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
	(put 'make 'scheme-number
		(lambda (x) (tag x)))
	(put 'equal? '(scheme-number scheme-number) 
		(lambda (x y) (equal? x y)))
	'done)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-polynomial-package)
	;; internal procedures
	;; representation of poly
	(define (make-poly variable term-list) (list variable term-list))
	(define (variable p) (car p))
	(define (term-list p) (cadr p))
	(define (same-variable? v1 v2)
		(equal? v1 v2))
	(define (raise-poly p target-var)
		(let ((var (variable p))
			  (terms (term-list p)))
			(make-poly target-var (raise-terms terms var target-var))))
	(define (add-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
			(make-poly (variable p1)
					   (add-terms (term-list p1) (term-list p2)))
			(if (high? (variable p1) (variable p2))
				(add-poly p1 (raise-poly p2 (variable p1)))
				(add-poly (raise-poly p1 (variable p2)) p2))))
	(define (mul-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
			(make-poly (variable p1) 
					   (mul-terms (term-list p1) (term-list p2)))
			(if (high? (variable p1) (variable p2))
				(mul-poly p1 (raise-poly p2 (variable p1)))
				(mul-poly (raise-poly p1 (variable p2)) p2))))
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
	(define (raise-term term cur-var new-var)
		(if (equal? 'polynomial (type-tag (coeff term)))
				(if (equal? (variable (contents (coeff term))) new-var)
				    (mul-terms (list (list 0 (make-polynomial cur-var (list (list (order term) 1))))) (term-list (contents (coeff term))))
					(list (list 0 (make-polynomial cur-var (list term)))))
				(list (list 0 (make-polynomial cur-var (list term))))))
    (define (raise-terms terms cur-var new-var)
		(let ((terms-list (map (lambda (term) (raise-term term cur-var new-var)) terms)))
			(batch-add-terms terms-list)))
	(define (batch-add-terms terms-list)
		(define (batch-add-terms-iter terms acc)
			(if (null? terms)
				acc
				(batch-add-terms-iter (cdr terms) (add-terms acc (car terms)))))
		(batch-add-terms-iter terms-list (list (list 0 0))))
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
	(put 'mul '(polynomial polynomial)
		(lambda (p1 p2) (tag (mul-poly p1 p2))))
	(put 'make 'polynomial
		(lambda (var terms) (tag (make-poly var terms))))
    (put 'equal-zero? '(polynomial) 
		(lambda (p) (if (empty-termlist? (term-list p))
						#t
						(myand (map equal-zero? (map coeff (term-list p)))))))
	'done)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(define (myand l)
	(cond ((null? l) #t)
		  ((car l) (myand (cdr l)))
		  (else #f)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (myinteger? z)
	(if (integer? z)
		(not (member #\. (string->list (number->string z))))
		#f))
		
(define (attach-tag tag contents)
	(cond ((myinteger? contents) contents)
	      ((real? contents) (* 1.0 contents))
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

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
			    (apply proc (map contents args))
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
	(install-polynomial-package)
	(install-hierarchies-package)
	(install-polynomials-hierarchies-package)
	(put 'raise '(scheme-number)
		(lambda (x) (make-polynomial 'x (list (list 0 x)))))
    (put 'equal-zero? '(scheme-number) 
		(lambda (x) (= x 0)))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))


(define (add v1 v2) (apply-generic 'add v1 v2))
(define (mul v1 v2) (apply-generic 'mul v1 v2))
(define (equal-zero? z) (apply-generic 'equal-zero? z))
(define (raise-value z) (apply-generic 'raise z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(install-generic-package)
(add (make-polynomial 'x (list (list 2 1))) 1)
(add (make-polynomial 'y (list (list 2 1))) 1)
(add (make-polynomial 'z (list (list 2 1))) 1)

(add (make-polynomial 'x (list (list 2 (make-polynomial 'y (list (list 2 1)))))) 
	 (make-polynomial 'y (list (list 1 1))))

(add (make-polynomial 'x (list (list 2 (make-polynomial 'z (list (list 2 1)))))) 
	 (make-polynomial 'z (list (list 1 1))))
	 
(add (make-polynomial 'x (list (list 2 (make-polynomial 'y (list (list 2 1)))))) 
	 (make-polynomial 'z (list (list 1 1))))
	 
(add (make-polynomial 'y (list (list 1 (make-polynomial 'x (list (list 2 1))))))
	 (add (make-polynomial 'x (list (list 2 (make-polynomial 'y (list (list 2 1)))))) 
		  (make-polynomial 'z (list (list 1 1)))))