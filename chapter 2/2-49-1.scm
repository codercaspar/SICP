(require (planet soegaard/sicp:2:1/sicp))

(define outline-segments
	(list
		(make-segment (make-vect 0.0 0.0)  (make-vect 0.0 0.99))
		(make-segment (make-vect 0.0 0.0)  (make-vect 0.99 0.0))
		(make-segment (make-vect 0.99 0.0) (make-vect 0.99 0.99))
		(make-segment (make-vect 0.0 0.99) (make-vect 0.99 0.99))))

(define outline (segments->painter outline-segments))