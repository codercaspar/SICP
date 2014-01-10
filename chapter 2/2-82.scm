;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-generic op .args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args))
				(if (> (length args) 1)
					(if (multi-equal? type-tags)
						(error "No method for these types" (list op type-tags))
						(try-all-coercion op type-tags args))
					(error "No method for these types" (list op type-tags)))))))
					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (try-all-coercion op type-tags args)
	(define (try-all-coercion-iter op type-tags args samples)
		(if (null? samples)
			(error "No method for these types" (list op type-tags))
			(let ((sample-type-tag (car samples))
				  (procs (map-get-coercion type-tags sample-type-tag)))
				(if (all-coercion-exist? procs type-tags sample-type-tag)
					(let ((all (coercion-all procs args)))
						(if (get op (map type-tag all))
							(apply-generic op all)
							(try-all-coercion-iter op type-tags args (cdr samples)))
					(try-all-coercion-iter op type-tags args (cdr samples)))))))
	(try-all-coercion-iter op type-tags args type-tags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
									  
(define (multi-equal? type-tags)
	(cond ((> (length type-tags) 2) 
				(let ((t1 (car type-tags))
					  (t2 (cadr type-tags)))
					(if (equal? t1 t2)
						(multi-equal? (caddr type-tags))
						#f)))
		   ((= (length type-tags) 2)
				(let ((t1 (car type-tags))
					  (t2 (cadr type-tags)))
					(equal? t1 t2)))
		   (else (error "No method for these types" (list op type-tags)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   
(define (map-get-coercion type-tags sample-type-tag)
	(map (lambda (type-tag) (get-coercion type-tag sample-type-tag)) type-tags))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (all-coercion-exist? procs type-tags sample-type-tag)
	(cond ((null? procs) (#t))
		  ((equal? (car type-tags) sample-type-tag)
				(all-coercion-exist? (cdr procs) (cdr type-tags) sample-type-tag))
		   (else
				(if (car procs)
					(all-coercion-exist? (cdr procs) (cdr type-tags) sample-type-tag)
					#f))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coercion-all procs args)
	(define (coercion-all-iter procs args acc)
		(cond ((null? procs) acc)
			  (if (car procs)
				  (coercion-all-iter (cdr procs) (cdr args) (append acc ((car procs) (car args))))
				  (coercion-all-iter (cdr procs) (cdr args) (append acc (car args))))))
	(coercion-all-iter procs args (list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;