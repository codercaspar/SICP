(define (an-integer-starting-from triple)
	(amb triple (an-integer-starting-from (next-item triple))))

(define (a-pythagorean-triple-start init-triple)
	(let ((triple (an-integer-starting-from init-triple)))
		(let ((i (car   triple))
			  (j (cadr  triple))
			  (k (caddr triple)))
		    (require (= (+ (* i i) (* j j)) (* k k)))
		    (list i j k))))

(define (require p)
	(if (not p) (amb)))

(define (next-item item)
	(let ((i (car   item))
		  (j (cadr  item))
		  (k (caddr item)))
	    (cond ((= i j k) (list 1 1 (+ k 1)))
	          ((= i j) (list 1 (+ j 1) k))
	          (else (list (+ i 1) j k)))))

(a-pythagorean-triple-start (list 1 1 1))