#lang scheme
(require scheme/mpair)

(define cdr mcdr)
(define car mcar)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)
(define cons mcons)
(define list mlist)
(define pair? mpair?)
(define list? mlist?)

(define (front-ptr queue) (cdr (car queue)))
(define (rear-ptr queue) (cdr (cdr queue)))
(define (set-front-ptr! queue item) (set-cdr! (car queue) item))
(define (set-rear-ptr! queue item) (set-cdr! (cdr queue) item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))