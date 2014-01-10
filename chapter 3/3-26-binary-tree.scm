;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-node key value) (cons key value))
(define (get-key node)   (car node))
(define (get-value node) (cdr node))
(define (set-value value node) (set-car! node value))

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

(define tree (cons 'tree '()))
(tree-insert 2 'b tree)
(tree-insert 1 'a tree)
(tree-insert 3 'c tree)
(tree-lookup 1 tree)
(tree-lookup 2 tree)
(tree-lookup 3 tree)