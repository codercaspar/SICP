(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define x-segments
 (list
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 0.99 0.99))
  (make-segment
   (make-vect 0.0 0.99)
   (make-vect 0.99 0.0))))

(define x-painter (segments->painter x-segments))