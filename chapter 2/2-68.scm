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

(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
			(list)
			(let ((next-branch (choose-branch (car bits) current-branch)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
					(decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))

(define (choose-branch bit branch)
	(cond ((= bit 0) (left-branch branch))
		  ((= bit 1) (right-branch branch))
		  (else (error "bad bit - CHOOSE-BRANCH" bit))))

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
	
(define (make-leaf symbol weight)
	(list 'leaf symbol weight))
	
(define (leaf? object)
	(eq? (car object) 'leaf))
	
(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sample-tree
	(make-code-tree 
		(make-leaf 'A 4)
		(make-code-tree
			(make-leaf 'B 2)
			(make-code-tree 
				(make-leaf 'D 1)
				(make-leaf 'C 1)))))
			
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;