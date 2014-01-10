(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


(define (square-limit painter n)
 (let ((corner (corner-split painter n)))
    (let ((tr (flip-horiz corner))
		  (bl (flip-vert corner))
		  (br (my-rotate-180 corner)))
		(below (beside bl br) (beside corner tr)))))
	 
(define (my-rotate-180 painter)
	((transform-painter (make-vect 1.0 1.0)   ; origin
                        (make-vect 0.0 1.0)   ; corner1
                        (make-vect 1.0 0.0))  ; corner2
    painter))
	 
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
	 
(paint (square-limit einstein 3))