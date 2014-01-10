(define (accumulate combiner null-value filter term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          (else (cond ((filter a) (iter (next a) (combiner result (term a))))
                      (else (iter (next a) result))))))
  (iter a null-value))

(define (prime? n)
   (= n (smallest-divisor n)))

(define (smallest-divisor n)
   (find-divisor n 2))

(define (find-divisor n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor n (+ test-divisor 1)))))

(define (square x)
   (* x x))

(define (divides? a b)
   (= (remainder b a) 0))

(define (sum-prime a b)
  (define (term x) x)
  (define (next x) (+ x 1))
  (accumulate + 0 prime? term a next b))