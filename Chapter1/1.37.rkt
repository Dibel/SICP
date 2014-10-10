#lang scheme

; 由以下Test部分可知k至少要取到12才能保证近似值具有十进制的4位精度

(define (cont-frac n d k)
  (cont-frac-iter n d k 0))

(define (cont-frac-iter n d k result)
  (if (= k 0)
      result
      (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) result)))))

; For test

; Result: 0.6180555555555556
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
             11)

; Result: 0.6180257510729613
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
             12)
