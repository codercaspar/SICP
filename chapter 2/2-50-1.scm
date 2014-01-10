(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (my-flip-horiz painter)
	((transform-painter (make-vect 1.0 0.0)   ; origin
                        (make-vect 0.0 0.0)   ; corner1
                        (make-vect 1.0 1.0))  ; corner2
    painter))
	
(paint (my-flip-horiz einstein))