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
						(operands exp)
						env)
		  			(begin
		  				(display exp)
		  				(newline)
		  				(error "Unknown expression type - EVAL" exp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (actual-value exp env)
	(force-it (my-eval exp env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (force-it obj)
	(if (thunk? obj)
		(actual-value (thunk-exp obj) (thunk-env obj))
		obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (delay-it exp env)
	(list 'thunk exp env))

(define (thunk? obj)
	(tagged-list? obj 'thunk))

(define (thunk-exp thunk) 
	(cadr thunk))

(define (thunk-env thunk) 
	(caddr thunk))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my-apply procedure arguments env)
	(cond ((primitive-procedure? procedure) 
			(apply-primitive-procedure procedure (list-of-arg-values arguments env)))
		  ((compound-procedure? procedure)
		  	(eval-sequence 
		  		(procedure-body procedure)
		  		(extend-environment 
		  			(procedure-parameters procedure) 
		  			(list-of-delayed-args arguments env)
		  			(procedure-environment procedure))))
		  (else
		  	(begin
		  		(display procedure)
		  		(newline)
		  		(error "Unknown procedure type - APPLY" procedure)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-of-arg-values exps env)
	(if (no-operands? exps)
		'()
		(cons 
			(actual-value (first-operand exps) env)
			(list-of-arg-values (rest-operands exps) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-of-delayed-args exps env)
	(if (no-operands? exps)
		'()
		(cons 
			(delay-it (first-operand exps) env)
			(list-of-delayed-args (rest-operands exps) env))))

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

;(define (eval-sequence exps env)
;	(cond ((last-exp? exps) (my-eval (first-exp exps) env))
;		  (else (actual-value (first-exp exps) env) 
;		  		(eval-sequence (rest-exps exps) env))))

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

(define (make-set! var val)
	(list 'set! var val))

(put 'eval 'set! eval-assignment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filter pred? input)
	(if (null? input)
		'()
		(if (pred? (car input))
			(append (list (car input)) (filter pred? (cdr input)))
			(filter pred? (cdr input)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'eval 'delay (lambda (exp env) (make-lambda '() (cdr exp))))

(put 'eval 'force (lambda (exp env) (my-eval (list (my-eval (cadr exp) env)) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scan-out-defines body)
	(define (analyse input-body)
		(let ((defines (filter (lambda (x) (and (definition? x) (definition-variable? x))) input-body))
			  (others  (filter (lambda (x) (not (and (definition? x) (definition-variable? x)))) input-body)))
			(if (null? defines)
				input-body
				(let ((let-syntax (map (lambda (x) 
											(if (pair? (definition-value x))
												(list (definition-variable x) (list 'delay (definition-value x)))
												(list (definition-variable x) (definition-value x))))
									   defines))
					  (set-syntax (map (lambda (x) 
					  						(make-set! 
					  							(definition-variable x) 
					  							(list 'force (definition-variable x))))
					  				   (filter (lambda (x) (pair? (definition-value x))) defines))))
					(list (make-let let-syntax (append set-syntax others)))))))
	(analyse body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-definition exp env)
	(define-variable! 
		(definition-variable exp)
		(my-eval (definition-value exp) env)
		env)
	'ok)

(define (definition? exp)
	(tagged-list? exp 'define))

(define (definition-variable? exp)
	(symbol? (cadr exp)))

(define (definition-variable exp)
	(if (definition-variable? exp)
		(cadr exp)
		(caadr exp)))

(define (definition-value exp)
	(if (definition-variable? exp)
		(caddr exp)
		(make-lambda 
			(cdadr exp) 	; formal parameters
			(cddr exp)))) 	; body

(define (make-define3 name var body)
	(cons 'define (cons (cons name var) body)))

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
	(if (true? (actual-value (if-predicate exp) env))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (while? exp)
	(tagged-list? exp 'while))

(define (while-predict exp)
	(cadr exp))

(define (while-body exp)
	(cddr exp))

(define (make-while predict body)
	(cons 'while (cons predict body)))

(define (while->combination exp)
	(let ((predict (while-predict exp))
		  (body (while-body exp)))
		(make-if 
			predict
			(make-begin (append body (list (make-while predict body))))
			'true)))

(put 'eval 'while (lambda (exp env) (my-eval (while->combination exp) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cond? exp)
	(tagged-list? exp 'cond))

(define (cond-clauses exp)
	(cdr exp))

(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else))

(define (cond-=>-clause? action)
	(eq? (car action) '=>))

(define (cond-=>-action action)
	(cdr action))

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
			  (rest  (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clause isn’t last - COND->IF" clauses))
				(let ((predicate (cond-predicate first))
					  (actions (cond-actions first)))
					(if (cond-=>-clause? actions)
						(make-if predicate
							(list (sequence->exp (cond-=>-action actions)) predicate)
							(expand-clauses rest))
						(make-if predicate (sequence->exp actions) (expand-clauses rest))))))))

(put 'eval 'cond (lambda (exp env) (my-eval (cond->if exp) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (let? exp) 
	(tagged-list? exp 'let))

(define (let-named? exp)
	(not (pair? (cadr exp))))

(define (let-not-named-var exp)
	(map car (cadr exp)))

(define (let-not-named-arg exp)
	(map cadr (cadr exp)))

(define (let-not-named-body exp)
	(cddr exp))

(define (let-named-proc exp)
	(cadr exp))

(define (let-named-var exp)
	(map car (caddr exp)))

(define (let-named-arg exp)
	(map cadr (caddr exp)))

(define (let-named-body exp)
	(cdddr exp))

(define (let->combination exp)
	(if (let-named? exp)
		(make-begin 
			(list
				(make-define3 (let-named-proc exp) (let-named-var exp) (let-named-body exp))
				(cons (let-named-proc exp) (let-named-arg exp))))
		(cons 
			(make-lambda (let-not-named-var exp) (let-not-named-body exp)) (let-not-named-arg exp))))

(define (make-let binds body)
	(cons 'let (cons binds body)))

(put 'eval 'let (lambda (exp env) (my-eval (let->combination exp) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (let*? exp) 
	(tagged-list? exp 'let*))

(define (let*-first-assign exp)
	(car (cadr exp)))

(define (let*-rest-assign exp)
	(cdr (cadr exp)))

(define (let*-last-assign? exp)
	(if (null? (let*-rest-assign exp))
		#t
		#f))

(define (let*-body exp)
	(cddr exp))

(define (let*->nested-lets exp)
	(let ((first (let*-first-assign exp))
		  (rest  (let*-rest-assign exp))
		  (body  (let*-body exp)))
		(if (null? rest)
			(let->combination (make-let (list first) body))
			(let->combination
				(make-let (list first) (list (let*->nested-lets (make-let* rest body))))))))

(define (make-let* binds body)
	(cons 'let* (cons binds body)))

(put 'eval 'let* (lambda (exp env) (my-eval (let*->nested-lets exp) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (true? x)
	(not (eq? x #f)))

(define (false? x)
	(eq? x #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-procedure parameters body env)
	(list 'procedure parameters (scan-out-defines body) env))

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
	(if (null? variables)
		'()
		(cons 
			(cons (car variables) (car values))
			(make-frame (cdr variables) (cdr values)))))

(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
		(cons (make-frame vars vals) base-env)
		(if (< (length vars) (length vals))
			(error "Too many arguments supplied" vars vals)
			(error "Too few arguments supplied" vars vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-binding-to-frame! var val frame env)
	(let ((new (cons (cons var val) frame)))
		(set-car! env new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup-variable-value var env)
	(if (eq? env the-empty-environment)
		(begin (display var) (newline) (error "Unbound variable" var))
		(let ((frame (first-frame env)))
			(let ((val (lookup-variable-value-in-environment var frame)))
				(if val
					(if (eq? val '*unassigned*)
						(error "Unassigned variable" var)
						val)
					(lookup-variable-value var (enclosing-environment env)))))))

(define (lookup-variable-value-in-environment var frame)
	(if (null? frame)
		#f
		(let ((frame-var (car (car frame)))
			  (frame-val (cdr (car frame))))
			(if (eq? var frame-var)
				frame-val
				(lookup-variable-value-in-environment var (cdr frame))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-variable-value! var val env)
	(if (eq? env the-empty-environment)
		(error "Unbound variable - SET!" var)
		(let ((frame (first-frame env)))
			(let ((old-val (lookup-variable-value-in-environment var frame)))
				(if old-val
					(set-variable-value-in-environment var val frame)
					(set-variable-value! var val (enclosing-environment env)))))))

(define (set-variable-value-in-environment var val frame)
	(if (null? frame)
		#f
		(let ((frame-var (car (car frame))))
			(if (eq? var frame-var)
				(set-car! frame (cons var val))
				(set-variable-value-in-environment var val (cdr frame))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(if (lookup-variable-value-in-environment var frame)
			(set-variable-value-in-environment var val frame)
			(add-binding-to-frame! var val frame env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unbound!? exp)
	(tagged-list? exp 'unbound!))

(define (eval-unbound! exp env)
	(if (eq? env the-empty-environment)
		(error "Unbound variable -unbound!" var)
		(let ((frame (first-frame env))
	  		  (var (cadr exp)))
		    (if (lookup-variable-value-in-environment var frame)
		    	(unbound!-frame var frame env)
		    	(eval-unbound! exp (enclosing-environment env))))))

(define (unbound!-frame var frame env)
		(if (null? frame)
			#f
			(let ((frame-var (car (car frame))))
				(if (eq? var frame-var)
					(begin (set! frame (cdr frame)) (set-car! env frame))
					(unbound!-frame-iter var frame (cdr frame))))))

(define (unbound!-frame-iter var prev-frame cur-frame)
	(let ((frame-var (car (car cur-frame))))
		(if (eq? var frame-var)
			(begin (set-cdr! prev-frame (cdr cur-frame)))
			(unbound!-frame-iter var cur-frame (cdr cur-frame)))))

(put 'eval 'unbound! eval-unbound!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (primitive-procedure? proc)
	(tagged-list? proc 'primitive))

(define apply-in-underlying-scheme 
	apply)

(define (primitive-implementation proc) 
	(cadr proc))

(define primitive-procedures
	(list (list 'car car)
		  (list 'cadr cadr)
		  (list 'cdr cdr)
		  (list 'cons cons)
		  (list 'null? null?)
		  (list '+ +)
		  (list '- -)
		  (list '* *)
		  (list '/ /)
		  (list '= =)
		  (list 'list list)
		  (list 'cons cons)
		  (list 'eq? eq?)
		  (list 'assoc assoc)
		  (list 'display display)))

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
		(let ((output (actual-value input the-global-environment)))
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