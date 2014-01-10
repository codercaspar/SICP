(define count 0)

(define (require p)
	(if (not p) (amb)))

(let ((x (amb 'a 'b 'c))
	  (y (amb 'a 'b 'c)))
    (set! count (+ count 1))
    (require (not (eq? x y)))
    (list x y count))