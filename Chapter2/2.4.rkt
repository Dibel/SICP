#lang scheme

; 可以对(car (cons x y))进行逐级展开
; 第一步：(car (lambda (m) (m x y)))
; 第二步：((lambda (m) (m x y)) (lambda (p q) p))
; 第三步：((lambda (p q) p) x y)
; 第四步：p
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))


(define (cdr z)
  (z (lambda (p q) q)))

; For test
(car (cons 4 5))
(cdr (cons 4 5))