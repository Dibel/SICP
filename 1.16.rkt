#lang scheme

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
