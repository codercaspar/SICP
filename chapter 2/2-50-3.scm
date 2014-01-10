(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (my-rotate-270 painter)
	((transform-painter (make-vect 0.0 1.0)   ; origin
                        (make-vect 0.0 0.0)   ; corner1
                        (make-vect 1.0 1.0))  ; corner2
    painter))
	
(paint (my-rotate-270 einstein))