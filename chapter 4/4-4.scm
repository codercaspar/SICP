;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
	(define (put-helper k array)
	(cond ((null? array) (list(make-entry k item)))
	      ((equal? (key (car array)) k) array)
	      (else (cons (car array) (put-helper k (cdr array))))))
	(set! global-array (put-helper (list op type) global-array)))

(define (get op type)
	(define (get-helper k array)
	(cond ((null? array) #f)
	      ((equal? (key (car array)) k) (value (car array)))
	      (else (get-helper k (cdr array)))))
	(get-helper (list op type) global-array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my-eval exp env)
	(cond ((self-evaluating? exp) 
			exp)
		  ((variable? exp) 
		  	(lookup-variable-value exp env))
		  (else 
		  	(if (get 'eval (get-tagged exp))
		  		((get 'eval (get-tagged exp)) exp env)
		  		(if (application? exp)
		  			(my-apply 
						(my-eval (operator exp) env)
						(list-of-values (operands exp) env))
		  			(error "Unknown expression type - EVAL" exp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my-apply procedure arguments)
	(cond ((primitive-procedure? procedure) 
			(apply-primitive-procedure procedure arguments))
		  ((compound-procedure? procedure) 
		  	(eval-sequence 
		  		(procedure-body procedure) 
		  		(extend-environment 
		  			(procedure-parameters procedure) 
		  			arguments
		  			(procedure-environment procedure))))
		  (else
		  	(error "Unknown procedure type - APPLY" procedure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-of-values exps env)
	(if (no-operands? exps)
		'()
		(cons (my-eval (first-operand exps) env) (list-of-values (rest-operands exps) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-sequence exps env)
	(cond ((last-exp? exps) 
			(my-eval (first-exp exps) env))
		  (else 
		  	(my-eval (first-exp exps) env) 
		  	(eval-sequence (rest-exps exps) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-assignment exp env)
	(set-variable-value! 
		(assignment-variable exp)
		(my-eval (assignment-value exp) env)
		env)
	'ok)

(define (assignment? exp)
	(tagged-list? exp 'set!))

(define (assignment-variable exp) 
	(cadr exp))

(define (assignment-value exp) 
	(caddr exp))

(put 'eval 'set! eval-assignment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-definition exp env)
	(define-variable! 
		(definition-variable exp)
		(my-eval (definition-value exp) env)
		env)
	'ok)

(define (definition? exp)
	(tagged-list? exp 'define))

(define (definition-variable exp)
	(if (symbol? (cadr exp))
		(cadr exp)
		(caadr exp)))

(define (definition-value exp)
	(if (symbol? (cadr exp))
		(caddr exp)
		(make-lambda 
			(cdadr exp) 	; formal parameters
			(cddr exp)))) 	; body

(put 'eval 'define eval-definition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (self-evaluating? exp)
	(cond ((number? exp) #t)
		  ((string? exp) #t)
		  (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (variable? exp) 
	(symbol? exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quoted? exp)
	(tagged-list? exp 'quote))

(define (text-of-quotation exp) 
	(cadr exp))

(put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		#f))

(define (get-tagged exp)
	(if (pair? exp)
		(car exp)
		#f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lambda? exp)
	(tagged-list? exp 'lambda))

(define (lambda-parameters exp)
	(cadr exp))

(define (lambda-body exp) 
	(cddr exp))

(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body)))

(put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (if? exp) 
	(tagged-list? exp 'if))

(define (if-predicate exp) 
	(cadr exp))

(define (if-consequent exp) 
	(caddr exp))

(define (if-alternative exp)
	(if (not (null? (cdddr exp)))
		(cadddr exp)
		'false))

(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative))

(define (eval-if exp env)
	(if (true? (my-eval (if-predicate exp) env))
		(my-eval (if-consequent exp) env)
		(my-eval (if-alternative exp) env)))

(put 'eval 'if eval-if)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begin? exp) 
	(tagged-list? exp 'begin))

(define (begin-actions exp) 
	(cdr exp))

(define (last-exp? seq) 
	(null? (cdr seq)))

(define (first-exp seq) 
	(car seq))

(define (rest-exps seq) 
	(cdr seq))

(define (sequence->exp seq)
	(cond ((null? seq) seq)
		  ((last-exp? seq) (first-exp seq))
		  (else (make-begin seq))))

(define (make-begin seq) 
	(cons 'begin seq))

(put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (application? exp) 
	(pair? exp))

(define (operator exp) 
	(car exp))

(define (operands exp) 
	(cdr exp))

(define (no-operands? ops) 
	(null? ops))

(define (first-operand ops) 
	(car ops))

(define (rest-operands ops) 
	(cdr ops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (and? exp)
	(eq? 'and (car exp)))
	
(define (eval-and exp env)
	(my-eval (and->if exp) env))

(define (and->if exp)
	(expand-and-booleans (and-booleans exp)))

(define (and-booleans exp)
	(cdr exp))

(define (expand-and-booleans booleans)
	(if (null? booleans)
		'true
		(let ((first (car booleans))
			  (rest  (cdr booleans)))
			(make-if first (expand-and-booleans rest) 'false))))

(put 'eval 'and eval-and)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (or? exp)
	(eq? 'or (car exp)))
	
(define (eval-or exp env)
	(my-eval (or->if exp) env))

(define (or->if exp)
	(expand-or-booleans (or-booleans exp)))

(define (or-booleans exp)
	(cdr exp))

(define (expand-or-booleans booleans)
	(if (null? booleans)
		'false
		(let ((first (car booleans))
			  (rest  (cdr booleans)))
			(make-if first 'true (expand-or-booleans rest)))))

(put 'eval 'or eval-or)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cond? exp)
	(tagged-list? exp 'cond))

(define (cond-clauses exp)
	(cdr exp))

(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) 
	(car clause))

(define (cond-actions clause) 
	(cdr clause))

(define (cond->if exp)
	(expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
	(if (null? clauses)
		'false ; no else clause
		(let ((first (car clauses))
			  (rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clause isn’t last - COND->IF" clauses))
				(make-if 
					(cond-predicate first)
					(sequence->exp (cond-actions first))
					(expand-clauses rest))))))

(put 'eval 'cond (lambda (exp env) (my-eval (cond->if exp) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (true? x)
	(not (eq? x #f)))

(define (false? x)
	(eq? x #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-procedure parameters body env)
	(list 'procedure parameters body env))

(define (compound-procedure? p)
	(tagged-list? p 'procedure))

(define (procedure-parameters p) 
	(cadr p))

(define (procedure-body p) 
	(caddr p))

(define (procedure-environment p) 
	(cadddr p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (enclosing-environment env) 
	(cdr env))

(define (first-frame env) 
	(car env))

(define the-empty-environment 
	'())

(define (make-frame variables values)
	(cons variables values))

(define (frame-variables frame) 
	(car frame))

(define (frame-values frame) 
	(cdr frame))

(define (add-binding-to-frame! var val frame)
	(set-car! frame (cons var (car frame)))
	(set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
		(cons (make-frame vars vals) base-env)
		(if (< (length vars) (length vals))
			(error "Too many arguments supplied" vars vals)
			(error "Too few arguments supplied" vars vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup-variable-value var env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond ((null? vars)
				  (env-loop (enclosing-environment env))) 
				  ((eq? var (car vars)) (car vals))
				  (else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error "Unbound variable" var)
			(let ((frame (first-frame env)))
				(scan (frame-variables frame) (frame-values frame)))))
	(env-loop env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-variable-value! var val env)
	(define (env-loop env)
		(define (scan vars vals)
			(cond ((null? vars) (env-loop (enclosing-environment env)))
				  ((eq? var (car vars)) (set-car! vals val))
				  (else (scan (cdr vars) (cdr vals)))))
		(if (eq? env the-empty-environment)
			(error "Unbound variable - SET!" var)
			(let ((frame (first-frame env)))
				(scan (frame-variables frame) (frame-values frame)))))
	(env-loop env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(define (scan vars vals)
			(cond ((null? vars) (add-binding-to-frame! var val frame))
				  ((eq? var (car vars)) (set-car! vals val))
				  (else (scan (cdr vars) (cdr vals)))))
		(scan (frame-variables frame) (frame-values frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (primitive-procedure? proc)
	(tagged-list? proc 'primitive))

(define apply-in-underlying-scheme 
	apply)

(define (primitive-implementation proc) 
	(cadr proc))

(define primitive-procedures
	(list (list 'car car)
		  (list 'cdr cdr)
		  (list 'cons cons)
		  (list 'null? null?)
		  (list '+ +)
		  (list '- -)
		  (list '* *)
		  (list '/ /)
		  (list 'list list)
		  (list 'cons cons)
		  (list 'eq? eq?)))

(define (primitive-procedure-names)
	(map car primitive-procedures))

(define (primitive-procedure-objects)
	(map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (setup-environment)
	(let ((initial-env (extend-environment 
							(primitive-procedure-names) 
							(primitive-procedure-objects)
							the-empty-environment)))
	(define-variable! 'true #t initial-env)
	(define-variable! 'false #f initial-env)
	initial-env))

(define the-global-environment 
	(setup-environment))

(define (apply-primitive-procedure proc args)
	(apply-in-underlying-scheme (primitive-implementation proc) args))

(define input-prompt 
	";;; M-Eval input:")

(define output-prompt 
	";;; M-Eval value:")

(define (driver-loop)
	(prompt-for-input input-prompt)
	(let ((input (read)))
		(let ((output (my-eval input the-global-environment)))
			(announce-output output-prompt)
			(user-print output)))
	(driver-loop))

(define (user-print object)
	(if (compound-procedure? object)
		(display (list 	'compound-procedure 
						(procedure-parameters object) 
						(procedure-body object)
						'<procedure-env>))
		(display object)))

(define (prompt-for-input string)
	(newline) 
	(newline) 
	(display string) 
	(newline))

(define (announce-output string)
	(newline) 
	(display string) 
	(newline))

(driver-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;