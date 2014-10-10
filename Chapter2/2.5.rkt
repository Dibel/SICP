#lang scheme

; 可以使用这种方法的原因是因为2和3都是质数
; 而对于一个正整数来说质因数分解的解是唯一的
; 因此在得到乘积后只需不断除以2或者3，记录下次数就可以得到对应的原始值
; 所以可以通过这种方式来表示序对

(define (cons x y)
  (* (fast-expt 2 x) (fast-expt 3 y)))

; 由于car和cdr过程相近，仅参数不同，故设compose过程以减小代码量
(define (compose z x)
  (define (iter value n)
    (if (not (= (remainder value x) 0))
        n
        (iter (/ value x) (+ n 1))))
  (iter z 0))

(define (car z)
  (compose z 2))

(define (cdr z)
  (compose z 3))

(define (fast-expt b n)
  (fastexpt-iter b n 1))

(define (fastexpt-iter b n a)
  (if (= n 0)
      a
      (if (even? n)
          (fastexpt-iter (square b) (/ n 2) a)
          (fastexpt-iter b (- n 1) (* b a)))))

(define (square n)
  (* n n))

; For test
(car (cons 4 5))
(cdr (cons 4 5))