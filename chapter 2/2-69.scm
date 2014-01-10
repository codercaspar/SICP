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

(generate-huffman-tree (list (make-leaf 'A 4)
							 (make-leaf 'B 2)
							 (make-leaf 'C 1)
							 (make-leaf 'D 1)))