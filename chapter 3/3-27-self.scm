(define (build-fib-proc)
	(let ((table (make-table)))
		(define (mem-fib n)
			(cond ((= n 0) 0)
				  ((= n 1) 1)
				  (else
					(let ((value ((table 'lookup-proc) (list n))))
						(if (equal? value '())
							(let ((value (+ (mem-fib (- n 1)) (mem-fib (- n 2)))))
								(begin
									((table 'insert-proc!) (list n) value)
									value))
							value)))))
		mem-fib))
	
(define (make-table)
	(let ((local-table (list (cons '*table* '()))))
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (define (myassoc key records)
			(cond ((null? records) #f) 
				  ((equal? key (caaar records)) (car records))
				  (else (myassoc key (cdr records)))))
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (define (lookup keys)
			(lookup-iter keys local-table))
		(define (lookup-iter keys table)
			(if (null? keys)
				(cdar table)
				(let ((subtable (myassoc (car keys) (cdr table))))
					(if subtable
						(lookup-iter (cdr keys) subtable)
						'()))))
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(define (insert! keys value)
			(insert-iter! keys value local-table))
		(define (insert-iter! keys value table)
			(if (null? keys)
			    (set-cdr! (car table) value)
				(let ((subtable (myassoc (car keys) (cdr table))))
					(if subtable
					    (insert-iter! (cdr keys) value subtable)
						(begin
						    (set-cdr! table (cons (cons (cons (car keys) '()) '()) (cdr table)))
							(insert-iter! keys value table))))))	
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(define (dispatch m)
			(cond ((eq? m 'lookup-proc)  lookup)
				  ((eq? m 'insert-proc!) insert!)
				  (else (error "Unknown operation - TABLE" m))))
		dispatch))

(define (error reason . args)
	(display "Error: ")
	(display reason)
	(for-each (lambda (arg) (display " ") (write arg)) args)
    (newline)
    (scheme-report-environment -1))
	
(define fib (build-fib-proc))
(fib 4)
(fib 21)