;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup-variable-value var env)
	(if (eq? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
			(let ((val (lookup-variable-value-in-environment var frame)))
				(if val
					val
					(lookup-variable-value var (enclosing-environment env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-variable-value! var val env)
	(if (eq? env the-empty-environment)
		(error "Unbound variable - SET!" var)
		(let ((frame (first-frame env)))
			(let ((val (lookup-variable-value-in-environment var frame)))
				(if val
					(set-variable-value-in-environment var val frame)
					(set-variable-value! var val (enclosing-environment env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(if (lookup-variable-value-in-environment var frame)
			(set-variable-value-in-environment var val frame)
			(add-binding-to-frame! var val frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;