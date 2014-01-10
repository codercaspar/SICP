(define (deriv expr var)
	(cond ((number? expr) 0)
		  ((variable? expr) (if (same-variable? expr var) 1 0))
		  (else ((get 'deriv (operator expr)) (operands expr) var))))
		  
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define (=number? expr num)
	(and (number? expr) (= expr num)))

(define (make-sum a1 a2) 
	(cond ((=number? a1 0) a2)
		  ((=number? a2 0) a1)
		  ((and (number? a1) (number? a2)) (+ a1 a2))
		  (else (list '+ a1 a2))))
  
(define (addend s) (car s))
(define (augend s) (cadr s))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define (make-product m1 m2) 
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
		  ((=number? m1 1) m2)
		  ((=number? m2 1) m1)
		  ((and (number? m1) (number? m2)) (* m1 m2))
		  (else (list '* m1 m2))))

(define (multiplier p)   (car p))
(define (multiplicand p) (cadr p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-deriv-packet)
	(define (sum expr var)
		(make-sum (deriv (addend expr) var)
				  (deriv (augend expr) var)))
	(define (product expr var)
		(make-sum	(make-product (multiplier expr) (deriv (multiplicand expr) var))
					(make-product (deriv (multiplier expr) var) (multiplicand expr))))
	(put 'deriv '+ sum)
	(put 'deriv '* product))
	
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
	
(install-deriv-packet)
(deriv '(+ x 3) 'x)
(deriv '(* x 3) 'x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;