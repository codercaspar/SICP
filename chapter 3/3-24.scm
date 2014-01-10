(define (make-table same-key?)
	(let ((local-table (list '*table*)))
	    (define (myassoc key records)
			(cond ((null? records) #f) 
				  ((same-key? key (caar records)) (car records))
				  (else (myassoc key (cdr records)))))
		(define (lookup key-1 key-2)
			(let ((subtable (myassoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (myassoc key-2 (cdr subtable))))
						(if record
							(cdr record)
							#f))
					#f)))
		(define (insert! key-1 key-2 value)
			(let ((subtable (myassoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (myassoc key-2 (cdr subtable))))
						(if record
							(set-cdr! record value)
							(set-cdr! subtable
								(cons (cons key-2 value) (cdr subtable)))))
					(set-cdr! local-table
						(cons (list key-1 (cons key-2 value)) (cdr local-table)))))
			'ok)
	(define (dispatch m)
		(cond ((eq? m 'lookup-proc)  lookup)
			  ((eq? m 'insert-proc!) insert!)
			  (else (error "Unknown operation - TABLE" m))))
	dispatch))
	
(define table (make-table (lambda (x y) (> 2 (- x y)))))
((table 'insert-proc!) 1 2 1)
((table 'lookup-proc) 1 2)
((table 'insert-proc!) 1 1.5 2)
((table 'lookup-proc) 1 2)