;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-frame variables values)
	(if (null? variables)
		'()
		(cons 
			(cons (car variables) (car values))
			(make-frame (cdr variables) (cdr values)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-binding-to-frame! var val frame)
	(set! frame (cons (cons var val) frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
		(cons (make-frame vars vals) base-env)
		(if (< (length vars) (length vals))
			(error "Too many arguments supplied" vars vals)
			(error "Too few arguments supplied" vars vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup-variable-value-in-environment var frame)
	(if (null? frame)
		#f
		(let ((frame-var (car (car frame)))
			  (frame-val (cdr (car frame))))
			(if (eq? var frame-var)
				frame-val
				(lookup-variable-value-in-environment var (cdr frame))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-variable-value-in-environment var val frame)
	(if (null? frame)
		(error "Too many arguments supplied" vars vals)
		(let ((frame-var (car (car frame))))
			(if (eq? var frame-var)
				(set-car! frame (cons var val))
				(lset-variable-value-in-environment var val frame)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-unbound! var frame)
	(if (null? frame)
		(error "no bound var" vars)
		(let ((frame-var (car (car frame))))
			(if (eq? var frame-var)
				(set! frame (cdr frame))
				(make-unbound! var frame)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;