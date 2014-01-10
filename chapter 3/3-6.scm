(define random-init 1)

(define rand
	(let ((x random-init))
		(lambda (action)
			(cond ((equal? action 'generate) (set! x (rand-update x)) x)
				  ((equal? action 'reset) (set! x random-init))
				  (else (error "error"))))))
		
(define (rand-update x)
	(modulo (+ (* 7 x) 17) 11))
	
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

(rand 'reset)

(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)