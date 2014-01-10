(define (make-frame origin edge1 edge2)
   (list origin edge1 edge2))

(define (origin-frame frame)
   (car frame))

(define (edge1-frame frame)
   (car (cdr frame)))

(define (edge2-frame frame)
   (car (cdr (cdr frame))))