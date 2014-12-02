#lang scheme

; My code begins at Line 50
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
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define ones (cons-stream 1 ones))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr s))
    (pairs (stream-cdr s) (stream-cdr t)))))

; My code

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2  weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car
                               (cons-stream s2car
                                            (merge-weighted
                                             (stream-cdr s1)
                                             (stream-cdr s2)
                                             weight)))))))))
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
weight)))

; For test

(define (display-stream s n)
  (if (= n 0)
      (void)
      (begin
        (display (stream-car s))(newline)
        (display-stream (stream-cdr s) (- n 1)))))

(define s1 (weighted-pairs
            integers
            integers
            (lambda (p) (+ (car p) (cadr p)))))
(display-stream s1 10)

(display "====newline====")(newline)(display "b)")(newline)(newline)

; 过滤出不能被2、3、5整除的数
(define (judge n)
  (define (divisible? x y) (= (remainder x y) 0))
  (if (or (divisible? n 2)
          (divisible? n 3)
          (divisible? n 5))
      #f
      #t))

(define filter-integers (stream-filter judge integers))

(define s2 (weighted-pairs
            filter-integers
            filter-integers
            (lambda (p)
              (let ((i (car p)) (j (cadr p)))
                (+ (* i 2) (* j 3) (* i j 5))))))


(display-stream s2 20)
