#lang scheme

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

; n次平滑
(define (smooth-n f n)
  ((repeated smooth n) f))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

; For test
(define (square x)
  (* x x))

((smooth square) 5)
((smooth-n square 5) 5)