#lang scheme

(define tolerance 0.00001)

(define (iterative-improve good-enough? improve)
   (define (try guess)
     (let ((next (improve guess)))
       (if (good-enough? guess next)
           next
           (try next))))
  (lambda (x) (try x)))

(define (sqrt x)
  (define (average v1 v2)
    (/ (+ v1 v2) 2))
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (improve guess)
        (f guess))
    ((iterative-improve close-enough? improve) first-guess))

; For test
(fixed-point cos 1.0)

(sqrt 5.0)