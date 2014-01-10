(define (a-pythagorean-triple-between low high)
	(let ((i (an-integer-between low high))
		  (hsq (* high high)))
	    (let ((j (an-integer-between i high)))
	    	(let ((ksq (+ (* i i) (* j j))))
	    		(require (>= hsq ksq))
	    		(let ((k (sqrt ksq)))
	    			(require (integer? k))
	    			(list i j k))))))

(define (require p)
	(if (not p) (amb)))

(define (an-integer-between low high)
	(require (<= low high))
	(amb low (an-integer-between (+ low 1) high)))

(a-pythagorean-triple-between 1 10)

确实效率更高了，因为原算法相当于三个for循环，是O(n^3)的时间复杂度，这个只要O(n^2)