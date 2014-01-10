(define (f x y)
    (cond ((= y 1) 1)
          ((= y x) 1)
          (else (+ (f (- x 1) (- y 1)) (f (- x 1) y)))))