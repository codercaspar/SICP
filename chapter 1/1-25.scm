(define (expmod base exp m)
	(remainder (fast-expt base exp) m))

(define (fast-expt b n)
   (cond ((= n 0) 1)
         ((even? n) (square (fast-expt b (/ n 2))))
         (else (* b (fast-expt b (- n 1))))))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
		  ((fermat-test n) (fast-prime? n (- times 1)))
		  (else false)))

(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
   (if (fast-prime? n 500)
       (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
   (display " *** ")
   (display elapsed-time))

(define (smallest-divisor n)
   (find-divisor n 2))

(define (find-divisor n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor n (next-test-divisor test-divisor)))))

(define (next-test-divisor test-divisor)
   (cond ((= test-divisor 2) (+ test-divisor 1))
         (else (+ test-divisor 2))))

(define (divides? a b)
   (= (remainder b a) 0))

(define (square x)
   (* x x))

(define (prime? n)
   (= n (smallest-divisor n)))

(define (search-for-primes start end)
   (cond ((even? start) (search-for-primes (+ start 1) end))
         (else (cond ((< start end) (timed-prime-test start) 
                                    (search-for-primes (+ start 2) end))))))