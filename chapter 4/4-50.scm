#lang r5rs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(#%require (only racket/base random))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ambeval exp env succeed fail)
	((analyze exp) env succeed fail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze exp)
	(cond ((self-evaluating? exp) (analyze-self-evaluating exp))
		  ((quoted? exp) (analyze-quoted exp))
		  ((variable? exp) (analyze-variable exp))
		  ((assignment? exp) (analyze-assignment exp))
		  ((definition? exp) (analyze-definition exp))
		  ((if? exp) (analyze-if exp))
		  ((lambda? exp) (analyze-lambda exp))
		  ((let? exp) (analyze (let->combination exp)))
		  ((begin? exp) (analyze-sequence (begin-actions exp)))
		  ((cond? exp) (analyze (cond->if exp)))
		  ((ramb? exp) (analyze-ramb exp))
		  ((application? exp) (analyze-application exp))
		  (else (error "Unknown expression type - ANALYZE" exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tagged-list? exp tag)
	(if (pair? exp)
		(equal? (car exp) tag)
		#f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begin? exp) 
	(tagged-list? exp 'begin))

(define (begin-actions exp) 
	(cdr exp))

(define (make-begin seq) 
	(cons 'begin seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nth n l)
	(if (eq? 0 n)
		(car l)
		(nth (- n 1) (cdr l))))

(define (rest a l)
	(if (eq? a (car l))
		(cdr l)
		(cons (car l) (rest a (cdr l)))))

(define (rchoose l)
	(let ((n (random (length l))))
		(nth n l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ramb? exp) 
	(tagged-list? exp 'ramb))

(define (ramb-choices exp) 
	(cdr exp))

(define (analyze-ramb exp)
	(let ((cprocs (map analyze (ramb-choices exp))))
		(lambda (env succeed fail)
			(define (try-next choices)
				(if (null? choices)
					(fail)
					(let ((choose-ele (rchoose choices)))
						(let ((rest-eles (rest choose-ele choices)))
							(choose-ele 
								env
								succeed
								(lambda () (try-next rest-eles)))))))
			(try-next cprocs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (self-evaluating? exp)
	(cond ((number? exp) #t)
		  ((string? exp) #t)
		  (else #f)))

(define (analyze-self-evaluating exp)
	(lambda (env succeed fail)
		(succeed exp fail)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quoted? exp)
	(tagged-list? exp 'quote))

(define (text-of-quotation exp) 
	(cadr exp))

(define (analyze-quoted exp)
	(let ((qval (text-of-quotation exp)))
		(lambda (env succeed fail)
			(succeed qval fail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (variable? exp) 
	(symbol? exp))

(define (analyze-variable exp)
	(lambda (env succeed fail)
		(succeed (lookup-variable-value exp env) fail)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lambda? exp)
	(tagged-list? exp 'lambda))

(define (lambda-parameters exp)
	(cadr exp))

(define (lambda-body exp) 
	(cddr exp))

(define (analyze-lambda exp)
	(let ((vars (lambda-parameters exp))
		  (bproc (analyze-sequence (lambda-body exp))))
		(lambda (env succeed fail)
			(succeed (make-procedure vars bproc env) fail))))

(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body)))

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

(define (analyze-if exp)
	(let ((pproc (analyze (if-predicate exp)))
		  (cproc (analyze (if-consequent exp)))
		  (aproc (analyze (if-alternative exp))))
		(lambda (env succeed fail)
			(pproc 
				env
				;; success continuation for evaluating the predicate
				;; to obtain pred-value
				(lambda (pred-value fail2)
					(if (true? pred-value)
						(cproc env succeed fail2)
						(aproc env succeed fail2)))
				;; failure continuation for evaluating the predicate
				fail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (analyze-sequence exps)
	(define (sequentially a b)
		(lambda (env succeed fail)
			(a 
				env
				;; success continuation for calling a
				(lambda (a-value fail2)
					(b env succeed fail2))
				;; failure continuation for calling a
				fail)))
	(define (loop first-proc rest-procs)
		(if (null? rest-procs)
			first-proc
			(loop (sequentially first-proc (car rest-procs)) (cdr rest-procs))))
	(let ((procs (map analyze exps)))
		(if (null? procs)
			(error "Empty sequence - ANALYZE")
			'ok)
		(loop (car procs) (cdr procs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (analyze-definition exp)
	(let ((var (definition-variable exp))
		  (vproc (analyze (definition-value exp))))
		(lambda (env succeed fail)
			(vproc 
				env 
				(lambda (val fail2) (define-variable! var val env) (succeed 'ok fail2))
				fail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assignment? exp)
	(tagged-list? exp 'set!))

(define (assignment-variable exp) 
	(cadr exp))

(define (assignment-value exp) 
	(caddr exp))

(define (analyze-assignment exp)
	(let ((var (assignment-variable exp))
		  (vproc (analyze (assignment-value exp))))
		(lambda (env succeed fail)
			(vproc 
				env
				(lambda (val fail2) ; *1*
					(let ((old-value (lookup-variable-value var env)))
						(set-variable-value! var val env)
						(succeed 'ok (lambda () ; *2*
										(set-variable-value! var old-value env) (fail2)))))
				fail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (let? exp) 
	(tagged-list? exp 'let))

(define (let-var exp)
	(map car (cadr exp)))

(define (let-arg exp)
	(map cadr (cadr exp)))

(define (let-body exp)
	(cddr exp))

(define (let->combination exp)
	(cons (make-lambda (let-var exp) (let-body exp)) (let-arg exp)))

(define (make-let binds body)
	(list 'let binds body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (application? exp) 
	(pair? exp))

(define (operator exp) 
	(car exp))

(define (operands exp) 
	(cdr exp))

(define (analyze-application exp)
	(let ((fproc (analyze (operator exp)))
		  (aprocs (map analyze (operands exp))))
		(lambda (env succeed fail)
			(fproc 
				env
				(lambda (proc fail2)
					(get-args 
						aprocs 
						env 
						(lambda (args fail3)
							(execute-application proc args succeed fail3))
						fail2))
				fail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-args aprocs env succeed fail)
	(if (null? aprocs)
		(succeed '() fail)
		((car aprocs) 
			env ;; success continuation for this aproc
			(lambda (arg fail2)
				(get-args 
					(cdr aprocs)
					env
					;; success continuation for
					;; recursive call to get-args
					(lambda (args fail3)
						(succeed (cons arg args) fail3))
					fail2))
			fail)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (true? x)
	(not (equal? x #f)))

(define (false? x)
	(equal? x #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (primitive-procedure? proc)
	(tagged-list? proc 'primitive))

(define apply-in-underlying-scheme 
	apply)

(define (primitive-implementation proc) 
	(cadr proc))

(define (apply-primitive-procedure proc args)
	(apply-in-underlying-scheme (primitive-implementation proc) args))

(define (compound-procedure? p)
	(tagged-list? p 'procedure))

(define (procedure-body p) 
	(caddr p))

(define (procedure-parameters p) 
	(cadr p))

(define (procedure-environment p) 
	(cadddr p))

(define (make-procedure parameters body env)
	(list 'procedure parameters body env))

(define (execute-application proc args succeed fail)
	(cond ((primitive-procedure? proc) 
			(succeed (apply-primitive-procedure proc args) fail))
		  ((compound-procedure? proc)
		  	((procedure-body proc)
		  		(extend-environment (procedure-parameters proc) args (procedure-environment proc))
		  		succeed
		  		fail))
		  (else (error "Unknown procedure type - EXECUTE-APPLICATION" proc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
	(define (internal-loop try-again)
		(prompt-for-input input-prompt)
		(let ((input (read)))
			(if (eq? input 'try-again)
				(try-again)
				(begin
					(newline)
					(display ";;; Starting a new problem: ")
					(ambeval 
						input
						the-global-environment
						;; ambeval success
						(lambda (val next-alternative)
							(announce-output output-prompt)
							(user-print val)
							(internal-loop next-alternative))
						;; ambeval failure
						(lambda ()
							(announce-output ";;; There are no more values of")
							(user-print input)
							(driver-loop)))))))
	(internal-loop
		(lambda ()
			(newline)
			(display ";;; There is no current problem")
			(driver-loop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prompt-for-input string)
	(newline) 
	(newline) 
	(display string) 
	(newline))

(define (announce-output string)
	(newline) 
	(display string) 
	(newline))

(define (user-print object)
	(if (compound-procedure? object)
		(display (list 	'compound-procedure 
						(procedure-parameters object) 
						(procedure-body object)
						'<procedure-env>))
		(display object)))

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
	(if (equal? env the-empty-environment)
		(error "Unbound variable" var)
		(let ((frame (first-frame env)))
			(let ((val (lookup-variable-value-in-environment var frame)))
				(if (not (eq? val "not_existed_in_frame"))
					(if (eq? val '*unassigned*)
						(error "Unassigned variable" var)
						val)
					(lookup-variable-value var (enclosing-environment env)))))))

(define (lookup-variable-value-in-environment var frame)
	(if (null? frame)
		"not_existed_in_frame"
		(let ((frame-var (car (car frame)))
			  (frame-val (cdr (car frame))))
			(if (equal? var frame-var)
				frame-val
				(lookup-variable-value-in-environment var (cdr frame))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-variable-value! var val env)
	(if (equal? env the-empty-environment)
		(error "Unbound variable - SET!" var)
		(let ((frame (first-frame env)))
			(let ((old-val (lookup-variable-value-in-environment var frame)))
				(if (not (eq? old-val "not_existed_in_frame"))
					(set-variable-value-in-environment var val frame)
					(set-variable-value! var val (enclosing-environment env)))))))

(define (set-variable-value-in-environment var val frame)
	(if (null? frame)
		#f
		(let ((frame-var (car (car frame))))
			(if (equal? var frame-var)
				(set-car! frame (cons var val))
				(set-variable-value-in-environment var val (cdr frame))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-variable! var val env)
	(let ((frame (first-frame env)))
		(let ((old-val (lookup-variable-value-in-environment var frame)))
			(if (not (eq? old-val "not_existed_in_frame"))
				(set-variable-value-in-environment var val frame)
				(add-binding-to-frame! var val frame env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define primitive-procedures
	(list (list 'car car)
		  (list 'cadr cadr)
		  (list 'caddr caddr)
		  (list 'cdr cdr)
		  (list 'cons cons)
		  (list 'random random)
		  (list 'length length)
		  (list 'null? null?)
		  (list 'write write)
		  (list '+ +)
		  (list '- -)
		  (list '* *)
		  (list '/ /)
		  (list '= =)
		  (list '> >)
		  (list '< <)
		  (list '<= <=)
		  (list '>= >=)
		  (list 'memq memq)
		  (list 'abs abs)
		  (list 'member member)
		  (list 'remainder remainder)
		  (list 'not not)
		  (list 'list list)
		  (list 'cons cons)
		  (list 'eq? eq?)
		  (list 'equal? equal?)
		  (list 'sqrt sqrt)
		  (list 'integer? integer?)
		  (list 'assoc assoc)
		  (list 'display display)
		  (list 'newline newline)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (error reason . args)
	(display "Error: ")
	(display reason)
	(for-each (lambda (arg) (display " ") (write arg)) args)
	(newline)
	(scheme-report-environment -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(driver-loop)