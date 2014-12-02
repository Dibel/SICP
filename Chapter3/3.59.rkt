#lang scheme

(define stream-null? null?)
(define the-empty-stream '())
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc . argstreams)
  (if (stream-null? argstreams)
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map
                          (cons proc (map stream-cdr
                                          argstreams))))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define ones (cons-stream 1 ones))

; 提供倒数序列 1/1, 1/2, 1/3, ...
(define div-integers (stream-map / ones (integers-starting-from 1)))

(define (integrate-series s)
  (stream-map * s div-integers))

; For test

(define s (integrate-series ones))
(stream-ref s 4)

(define cosine-series
  (cons-stream 1 (stream-map (lambda (x) (- 0 x)) (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))