(define (id x) x)
(print (id 4))

(define (f x y) (+ x y))
(define (g x) (f x x))
(print (f 4 5))

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(print (fib (read-num)))

(define (even n) (if (zero? n) true (odd (sub1 n))))
(define (odd n) (if (zero? n) false (even (sub1 n))))
(print (even (read-num)))

