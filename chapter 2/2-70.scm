;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))
	
(define (successive-merge pairs)
	(if (= 1 (length pairs))
		(car pairs)
		(let ((first-min  (car pairs))
			  (second-min (cadr pairs)))
			(let ((new-code-tree (make-code-tree first-min second-min)))
				(successive-merge (make-leaf-set (append (list new-code-tree) (cddr pairs))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (adjoin-set x set)
	(cond ((null? set) (list x))
		  ((< (weight x) (weight (car set))) (cons x set))
		  (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
	(if (null? pairs)
		(list)
		(let ((pair (car pairs)))
			(adjoin-set pair (make-leaf-set (cdr pairs))))))
						
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
(define (make-leaf symbol weight)
	(list 'leaf symbol weight))
	
(define (leaf? object)
	(eq? (car object) 'leaf))
	
(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-code-tree left right)
	(list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))
	
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)))

(define (weight tree)
	(if (leaf? tree)
		(weight-leaf tree)
		(cadddr tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
		
(define (encode message tree)
	(if (null? message)
	(list)
	(append 
		(encode-symbol (car message) tree)
		(encode (cdr message) tree))))
		
(define (encode-symbol symbol tree)
	(cond ((and (leaf? tree) (eq? symbol (symbol-leaf tree))) (list))
		  ((and (leaf? tree) (not (eq? symbol (symbol-leaf tree)))) (error "bad bit - CHOOSE-BRANCH" symbol))
		  ((in-tree? symbol (left-branch tree)) (cons 0 (encode-symbol symbol (left-branch tree))))
		  (else (cons 1 (encode-symbol symbol (right-branch tree))))))
		  
(define (in-tree? symbol tree)
	(if (leaf? tree)
		(eq? symbol (symbol-leaf tree))
		(in-list? symbol (symbols tree))))
	
(define (in-list? x l)
	(cond ((null? l) #f)
		  ((eq? x (car l)) #t)
		  (else (in-list? x (cdr l)))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define codes (list (make-leaf 'A 2)
					(make-leaf 'BOOM 1)
					(make-leaf 'GET 2)
					(make-leaf 'JOB 2)
					(make-leaf 'NA 16)
					(make-leaf 'SHA 3)
					(make-leaf 'YIP 9)
					(make-leaf 'WAH 1)))
					
(define text '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH 
			   YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(encode text (generate-huffman-tree codes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;