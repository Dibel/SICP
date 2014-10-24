#lang racket/load

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-vect x y)
  (cons x y))

(define s1 (make-segment (make-vect 1.0 2.0) (make-vect 6.0 5.0)))