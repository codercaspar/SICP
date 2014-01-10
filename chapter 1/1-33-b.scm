(define (accumulate combiner null-value filter term a next b)
  (define (iter a result)
    (cond ((>= a b) result)
          (else (cond ((filter a b) (iter (next a) (combiner result (term a))))
                      (else (iter (next a) result))))))
  (iter a null-value))

(define (my_gcd a b)
	(if (= b 0)
		a
		(my_gcd b (remainder a b))))

(define (relative-prime? a b) 
	(= 1 (my_gcd b a)))

(define (product-relative-prime n)
  (define (term x) x)
  (define (next x) (+ x 1))
  (accumulate * 1 relative-prime? term 2 next n))