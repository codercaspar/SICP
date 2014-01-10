(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (rotate-90 painter)
 ((transform-painter (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0))
   painter))
   
(define (my-rotate-270 painter)
	((transform-painter (make-vect 0.0 1.0)   ; origin
                        (make-vect 0.0 0.0)   ; corner1
                        (make-vect 1.0 1.0))  ; corner2
    painter))
	
(define (below painter1 painter2)
	(rotate-90 (beside (my-rotate-270 painter1)
					   (my-rotate-270 painter2))))
				
(paint (below einstein einstein))