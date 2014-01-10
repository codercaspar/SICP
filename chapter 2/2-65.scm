;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (adjoin-set-tree x set)
	(let ((set-list (tree->list-2 set)))
		(list->tree (adjoin-set-list x set-list))))
			  
(define (adjoin-set-list x set)
	(cond ((null? set) (list x))
		  ((= x (car set)) set)
		  ((< x (car set)) (cons x set))
		  ((> x (car set)) (cons (car set) (adjoin-set-list x (cdr set))))))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (intersection-set-tree set1 set2)
	(let ((set1-list (tree->list-2 set1))
		  (set2-list (tree->list-2 set2)))
		(list->tree (intersection-set-list set1-list set2-list))))

(define (intersection-set-list set1 set2)
	(cond ((null? set1) (list))
	      ((null? set2) (list))
		  ((= (car set1) (car set2)) (cons (car set1) (intersection-set-list (cdr set1) (cdr set2))))
		  ((< (car set1) (car set2)) (intersection-set-list (cdr set1) set2))
		  ((> (car set1) (car set2)) (union-set-list set1 (cdr set2)))))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (union-set-tree set1 set2)
	(let ((set1-list (tree->list-2 set1))
		  (set2-list (tree->list-2 set2)))
		(list->tree (union-set-list set1-list set2-list))))

(define (union-set-list set1 set2)
	(cond ((null? set1) set2)
	      ((null? set2) set1)
		  ((= (car set1) (car set2)) (cons (car set1) (union-set-list (cdr set1) (cdr set2))))
		  ((< (car set1) (car set2)) (cons (car set1) (union-set-list (cdr set1) set2)))
		  ((> (car set1) (car set2)) (cons (car set2) (union-set-list set1 (cdr set2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  
(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
			result-list
			(copy-to-list (left-branch tree)
						  (cons (entry tree) (copy-to-list (right-branch tree)
														   result-list)))))
	(copy-to-list tree (list)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(define (list->tree elements)
	(car (partial-tree elements (length elements))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
(define (partial-tree elements n)
	(if (= n 0)
		(cons (list) elements)
		(let ((left-size (quotient (- n 1) 2)))
			 (let ((left-result (partial-tree elements left-size)))
				  (let ((left-tree (car left-result))
						(non-left-eles (cdr left-result))
						(right-size (- n (+ left-size 1))))
					(let ((this-entry (car non-left-eles))
						  (right-result (partial-tree (cdr non-left-eles) right-size)))
						(let ((right-tree (car right-result))
							  (remaining-eles (cdr right-result)))
							(cons (make-tree this-entry left-tree right-tree) remaining-eles))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
							
(define (entry tree)        (car tree))
(define (left-branch tree)  (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
	(list entry left right))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tree1  (make-tree 1 (list) (list)))
(define tree5  (make-tree 5 (list) (list)))
(define tree3  (make-tree 3 tree1 tree5))
(define tree11 (make-tree 11 (list) (list)))
(define tree9  (make-tree 9 (list) tree11))
(define tree7  (make-tree 7 tree3 tree9))