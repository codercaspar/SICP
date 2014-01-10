;;;;;scheme-number
(put 'zero? '(scheme-number) (lambda (x) (eq? x 0)))
;;;;;rational
(put 'zero? '(rational) (lambda (x) (and (eq? (numer x) 0) (not (eq? (denom x) 0)))))
;;;;;complex
(put 'zero? '(complex) (lambda (x) (and (eq? (real-part1 x) 0) (eq? (imag-part1 x) 0))))