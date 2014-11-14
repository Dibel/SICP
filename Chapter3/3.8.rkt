#lang scheme

(define f
  (lambda (x)
    (set! f (lambda (x) 0))
    x))

; For test

(+ (f 0) (f 1))