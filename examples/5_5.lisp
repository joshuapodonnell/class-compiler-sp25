(define (times x y)
(if (zero? y)
0
(+ x (times x (sub1 y)))))
(let ((x (- 9 (+ 3 5))))
(+ 3 (times x 4)))