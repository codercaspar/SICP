(define (sum term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))))
	(iter a 0))

(define (integral f a b n)
	(define h (/ (- b a) n))
	(define (add x) (+ x 1))
	(define (y k) (f (+ a (* k h))))
	(define (term k)
		(* (cond ((or (= k 0) (= k n)) 1)
			     ((even? k) 2)
			     (else 4))
		(y k)))
	(* (sum term 0 add n) (/ h 3)))

(define (cube x) (* x x x))