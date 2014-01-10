(define (yachts)
	(let ((Mary      (amb 'Mr-Moore))
		  (Melissa   (amb 'Mr-Barnacle))
		  (Gabrielle (amb 'Mr-Downing 'Mr-Hall))
		  (Rosalind  (amb 'Mr-Downing 'Mr-Parker))
		  (Lorna     (amb 'Mr-Downing 'Mr-Hall 'Mr-Parker)))
	    (require 
            (cond ((eq? Gabrielle 'Mr-Downing) (eq? Melissa 'Mr-Parker))
                  ((eq? Gabrielle 'Mr-Hall) (eq? Rosalind 'Mr-Parker))
                  (else false)))
	    (require (distinct? (list Mary Melissa Gabrielle Rosalind Lorna)))
	    (list 
	    	(list 'Mary Mary) 
	    	(list 'Melissa Melissa)
	    	(list 'Gabrielle Gabrielle)
	    	(list 'Rosalind Rosalind)
	    	(list 'Lorna Lorna))))

(define (require p)
	(if (not p) (amb)))

(define (distinct? items)
	(cond ((null? items) true)
		  ((null? (cdr items)) true)
		  ((member (car items) (cdr items)) false)
		  (else (distinct? (cdr items)))))

(yachts)