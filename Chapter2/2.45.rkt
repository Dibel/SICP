#lang racket/load

(load "drawing.rkt")

(define (split op1 op2)
  (define iter (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller (iter painter (- n 1))))
        (op1 painter (op2 smaller smaller)))))
  )
  iter)

(define up-split (split below beside))

(define right-split (split beside below))

(define (start-drawing)  ((right-split wave 2) testFrame))