#lang scheme

(define (fast-multi a b)
  (fastmulti-iter a b 0))

(define (fastmulti-iter b n a)
  (if (= n 0)
      a
      (if (even? n)
          (fastmulti-iter (double b) (halve n) a)
          (fastmulti-iter b (- n 1) (+ b a)))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))