;;;;;scheme-number
(put 'equ '(scheme-number scheme-number) (lambda (x y) (eq? x y)))
;;;;;rational
(put 'equ '(rational rational) (lambda (x y) (and (eq? (numer x) (numer y)) (eq? (denom x) (denom y)))))
;;;;;complex
(put 'equ '(complex complex) (lambda (x y) (and (eq? (real-part1 x) (real-part1 y)) 
												(eq? (imag-part1 x) (imag-part1 y)))))
