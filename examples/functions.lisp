(define (sum-tail n acc)
 (if (zero? n)
  acc
  (sum-tail (sub1 n) (+ n acc))))

(print (sum-tail (read-num) 0))




