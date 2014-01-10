(define (make-table)
	(let ((local-node (make-node 'table '())))
	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (define (myassoc key tree)
		    (tree-lookup key tree))
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (define (lookup keys)
			(if (null? keys)
				'()
				(lookup-iter keys local-node)))
		(define (lookup-iter keys node)
			(if (null? keys)
				(get-value node)
				(let ((cur-node (myassoc (car keys) (get-subtree node))))
					(if cur-node
						(lookup-iter (cdr keys) cur-node)
						'()))))
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(define (insert! keys value)
			(insert-iter! keys value local-node))
		(define (insert-iter! keys value node)
			(if (null? keys)
			    (set-value node value)
				(let ((cur-node (myassoc (car keys) (get-subtree node))))
					(if cur-node
					    (insert-iter! (cdr keys) value cur-node)
						(begin
						    (tree-insert (car keys) '() (get-subtree node))
							(insert-iter! keys value node))))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-subtree node sub) (set-car! (cddr node) sub))
(define (set-value node value) (set-car! (cdr node) value))
(define (get-key node) (car node))
(define (get-value node) (cadr node))
(define (get-subtree node) (caddr node))
(define (make-node key value)
	(list key value (cons 'tree '())))

(define (set-left tree new)  (set-car! (cdr tree) new))
(define (set-right tree new) (set-car! (cddr tree) new))
(define (get-node tree)     (car tree))
(define (left-branch tree)  (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree node left right) (list node left right))

(define (tree-lookup key tree)
	(tree-lookup1 key (cdr tree)))

(define (tree-lookup1 key tree)
	(cond ((null? tree) #f)
		  (else
			(let ((cur-key (get-key (get-node tree))))
				(cond ((= key cur-key) (get-node tree))
					  ((< key cur-key) (tree-lookup1 key (left-branch tree)))
					  (else (tree-lookup1 key (right-branch tree))))))))
				  
(define (tree-insert1 key value tree)
	(let ((cur-key (get-key (get-node tree))))
		(cond ((= key cur-key) (set-value (get-node tree)))
			  ((< key cur-key)
				(if (equal? (left-branch tree) '())
					(set-left tree (make-tree (make-node key value) '() '()))
					(tree-insert1 key value (left-branch tree))))
			  ((> key cur-key)
				(if (equal? (right-branch tree) '())
					(set-right tree (make-tree (make-node key value) '() '()))
					(tree-insert1 key value (right-branch tree)))))))
				  
(define (tree-insert key value tree-struct)
	(let ((tree (cdr tree-struct)))
		(if (equal? tree '())
			(set-cdr! tree-struct (make-tree (make-node key value) '() '()))
			(tree-insert1 key value (cdr tree-struct)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define table (make-table))
((table 'insert-proc!) (list 1 2) 1)
((table 'lookup-proc) (list 1 2))
((table 'lookup-proc) (list 1))
((table 'lookup-proc) (list 1 2 3))
((table 'insert-proc!) (list 1 2 3) 2)
((table 'lookup-proc) (list 1 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;