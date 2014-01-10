(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (below painter1 painter2)
	(let ((split-point (make-vect 0.0 0.5)))
		(let ((paint-top 
						((transform-painter split-point
											(make-vect 1.0 0.5)
											(make-vect 0.0 1.0))
							painter1))
			  (paint-bottom 
						((transform-painter (make-vect 0.0 0.0)
											(make-vect 1.0 0.0)
											split-point)
							painter2)))
			(lambda (frame)
				(paint-top    frame)
				(paint-bottom frame)))))
				
(paint (below einstein einstein))