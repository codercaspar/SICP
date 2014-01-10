((lambda (n)
	((lambda (fact) (fact fact n))
	 (lambda (ft k)
	 	(if (= k 1)
	 		1
	 		(* k (ft ft (- k 1)))))))
 10)


((lambda (n)
	((lambda (fact) (fact fact n))
	 (lambda (ft k)
	 	(cond ((= k 0) 1)
	 		  ((= k 1) 1)
	 		  (else (+ (ft ft (- k 2)) (ft ft (- k 1))))))))
 10)