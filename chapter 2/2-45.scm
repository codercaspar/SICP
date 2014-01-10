(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split dir1 dir2)
   (lambda (painter n)
     (if (= n 0)
         painter
         (let ((smaller ((split dir1 dir2) painter (- n 1))))
           (dir1 painter (dir2 smaller smaller))))))