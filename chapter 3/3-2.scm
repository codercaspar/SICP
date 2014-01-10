(define (make-monitored f)
	(let ((counter 0))
		(lambda (arg)
			(cond ((eq? arg 'reset-count) (set! counter 0))
				  ((eq? arg 'how-many-calls) counter)
				  (else (begin (set! counter (+ counter 1)) (f arg)))))))
				  
(define s (make-monitored sqrt))

(s 100)
(s 10000)
(s 'how-many-calls)
(s 100)
(s 'how-many-calls)
(s 'reset-count)
(s 'how-many-calls)