(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			(sum term (next a) next b))))

(define (integral f a b n)
	(define (add n) (+ n 1))
	(define (y k) (+ a (* k (/ (- b a) n))))
	(define (term k)
		(cond  ((= k 0) (f (y k)))
			   ((= k n) (f (y k)))
			   ((even? k) (* 2 (f (y k))))
			   (else (* 4 (f (y k))))))
	(* (sum term 0 add n) (/ (/ (- b a) n) 3)))

(define (cube x) (* x x x))