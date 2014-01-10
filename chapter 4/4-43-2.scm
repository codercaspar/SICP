(define (yachts)
	(let ((Melissa   (amb 'Mr-Barnacle))
		  (Gabrielle (amb 'Mr-Downing 'Mr-Hall 'Mr-Moore))
		  (Rosalind  (amb 'Mr-Downing 'Mr-Parker 'Mr-Moore))
		  (Lorna     (amb 'Mr-Downing 'Mr-Hall 'Mr-Parker 'Mr-Moore))
		  (Mary      (amb 'Mr-Downing 'Mr-Hall 'Mr-Parker 'Mr-Moore)))
	    (require 
            (cond ((eq? Gabrielle 'Mr-Downing) (eq? Melissa 'Mr-Parker))
                  ((eq? Gabrielle 'Mr-Hall) (eq? Rosalind 'Mr-Parker))
                  ((eq? Gabrielle 'Mr-Moore) (eq? Lorna 'Mr-Parker))
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