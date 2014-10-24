#lang racket/load

; list

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

; cons

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

; For test

(define (make-vect x y)
  (cons x y))

(define frame (make-frame (make-vect 1.0 2.0) (make-vect 2.0 2.0) (make-vect 5.0 2.0)))