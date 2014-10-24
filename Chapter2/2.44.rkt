#lang racket/load

(load "drawing.rkt")

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (start-drawing)  ((up-split wave 2) testFrame))
