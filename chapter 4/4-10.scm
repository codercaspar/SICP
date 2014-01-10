;修改4.6的例子，让let里的var和arg反过来一下

(define (let? exp) 
	(tagged-list? exp 'let))

(define (let-var exp)
	(map cadr (cadr exp)))

(define (let-arg exp)
	(map car (cadr exp)))

(define (let-body exp)
	(cddr exp))

(define (let->combination exp)
	(cons (make-lambda (let-var exp) (let-body exp)) (let-arg exp)))

(define (eval-let exp env)
	(eval (let->combination exp) env))

(define (make-let binds body)
	(list 'let binds body))