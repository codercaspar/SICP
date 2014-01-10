;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;origin;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-of-values exps env)
	(if (no-operands? exps)
		'()
		(cons 
			(eval (first-operand exps) env)
			(list-of-values (rest-operands exps) env))))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;l -> r;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-of-values exps env)
	(define (list-of-values-iter exps env rtn)
		(if (no-operands? exps)
			rtn
			(let ((value (my-eval (first-operand exps) env)))
				(list-of-values-iter (rest-operands exps) env (append rtn (list value))))))
	(list-of-values-iter exps env '()))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;r -> l;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-of-values exps env)
    (if (no-operands? exps)
    	'()
    	(let ((value (list-of-values (rest-operands exps) env)))
				(append (list (my-eval (first-operand exps) env)) value))))