(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define diamond-segments
 (list
  (make-segment
   (make-vect 0.0 0.5)
   (make-vect 0.5 0.0))
  (make-segment
   (make-vect 0.0 0.5)
   (make-vect 0.5 0.999))
  (make-segment
   (make-vect 0.5 0.999)
   (make-vect 0.999 0.5))
  (make-segment
   (make-vect 0.999 0.5)
   (make-vect 0.5 0.0))))

(define diamond (segments->painter diamond-segments))