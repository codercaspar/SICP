(define (expmod base exp m)
	(cond ((= exp 0) 1)
	      ((even? exp)
		  (remainder (square (expmod base (/ exp 2) m)) m))
	      (else
		  (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test? n a)
	(= (expmod a n n) a))

(define (test n)
	(test-iter n (- n 1)))

(define (test-iter a b)
	(cond ((= 0 b) 1)
	      ((fermat-test? a b) (test-iter a (- b 1)))
              (else 0)))

(define (square x) (* x x))