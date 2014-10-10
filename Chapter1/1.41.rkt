#lang scheme

(define (double procedure)
  (lambda (x) (procedure (procedure x))))

(define (inc n)
  (+ n 1))

; For test
; 此处相当于进行了4次double操作
; Result: 21

(((double (double double)) inc) 5)