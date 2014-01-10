(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (corner-split painter n)
 (if (= n 0)
     painter
     (let ((up (up-split painter (- n 1)))
           (right (right-split painter (- n 1)))
           (corner (corner-split painter (- n 1))))
         (beside (below painter up)
                 (below right corner)))))
				 
(define (up-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (up-split painter (- n 1))))
			(below painter (beside smaller smaller)))))
			
(define (right-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (right-split painter (- n 1))))
			(beside painter (below smaller smaller)))))
				 
(paint (corner-split einstein 4))