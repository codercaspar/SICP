(define (timed-prime-test n)
	(newline)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
   (if (prime? n)
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