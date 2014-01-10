(define (make-serializer n)
	(let ((mutex (make-mutex n)))
		(lambda (p)
			(define (serialized-p . args)
				(mutex 'acquire)
				(let ((val (apply p args)))
					(mutex 'release)
					val))
			serialized-p)))

(define (make-mutex n)
	(let ((cell (list n)))
		(define (the-mutex m)
			(cond ((eq? m 'acquire)
					(if (test-and-set! cell)
						(the-mutex 'acquire))) ; retry
				  ((eq? m 'release) (clear! cell))))
		the-mutex))

(define (clear! cell)
	(let ((num (car cell)))
		(set-car! cell (+ num 1))))

(define (test-and-set! cell)
	(let ((num (car cell)))
		(if (= num 0)
			#t
			(begin
				(set-car! cell (- num 1)
				#f)))))