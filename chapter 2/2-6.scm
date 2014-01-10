(define zero
	(lambda (f) (lambda (x) x)))
	
(define (add-1 n)
	(lambda (f) (lambda (x) (f ((n f) x)))))
	
(define one (add-1 zero))

(define two (add-1 one))

(define (inc x) (+ x 1))

(define (add m n)
	(lambda (f) (lambda (x) ((m f) ((n f) x)))))