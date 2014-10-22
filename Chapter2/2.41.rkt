#lang scheme

(define (sum-pairs n)
  (define (useful? pair)
    (let ((first (car pair))
          (second (cadr pair))
          (third (- n (car pair) (cadr pair))))
      (not (or (= third first) (= third second) (= first second)))))
  (define (make-pair pair)
    (list (car pair) (cadr pair) (- n (car pair) (cadr pair))))
  (map make-pair
       (filter useful?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 2 (- n i 1))))
                (enumerate-interval 1 (- n 3))))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

(define (filter predicate seq)
  (cond ((null? seq) '())
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(sum-pairs 20)