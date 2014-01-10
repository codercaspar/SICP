;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (serialized-exchange account1 account2)
	(let ((account1-id ((get-id account1)))
	      (account2-id ((get-id account2)))
		  (serializer1 (account1 'serializer))
		  (serializer2 (account2 'serializer)))
		(cond ((< account1-id account2-id)
				((serializer1 (serializer2 exchange)) account1 account2))
			  ((> account1-id account2-id)
				((serializer2 (serializer1 exchange)) account1 account2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-account balance)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount)) balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount)) balance)
	(let ((id (new-count-id))
		  (protected (make-serializer)))
		(define (dispatch m)
			(cond ((eq? m 'withdraw) (protected withdraw))
				  ((eq? m 'deposit) (protected deposit))
				  ((eq? m 'get-id) id)
				  (else (error "Unknown request - MAKE-ACCOUNT" m))))
		(list id dispatch)))
		
(define (get-id account) (account 'get-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
(define (make-account-id)
	(let ((id-pool 1))
		(define (dispatch m)
			(cond ((eq? m 'new-id)
					(let ((return id-pool))
						(begin
							(set! id-pool (+ id-pool 1))
							return)))
				  (else (error "Unknown request - MAKE-ACCOUNT" m))))
		dispatch))
		
(define serial-pool
	((make-serializer) (make-account-id)))

(define (new-count-id)
	(serial-pool 'new-id))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-serializer)
	(let ((mutex (make-mutex)))
		(lambda (p)
			(define (serialized-p . args)
				(mutex 'acquire)
				(let ((val (apply p args)))
					(mutex 'release)
					val))
			serialized-p)))
			
(define (make-mutex)
	(let ((cell (list false)))
		(define (the-mutex m)
			(cond ((eq? m 'acquire)
					(if (test-and-set! cell)
						(the-mutex 'acquire))) ; retry
				  ((eq? m 'release) (clear! cell))))
		the-mutex))

(define (clear! cell)
	(set-car! cell #f))

(define (test-and-set! cell)
	(if (car cell)
		#t
		(begin 
			(set-car! cell #t)
			#f)))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;