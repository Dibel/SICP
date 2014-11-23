#lang scheme

(define (count-pairs x)
    (let ((achieve '()))
      (define (iter x)
        (cond ((not (pair? x)) 0)
              (else
               (set! achieve (cons (cons '() x) achieve))
               (if (inlist x achieve)
                   0
                   (+ (iter (car x))
                      (iter (cdr x))
                      1)))))
      (iter x)))



(define (inlist item lst)
  (> (filter (lambda (x) (eq? item (cdr x))) lst) 1))

; 修改的filter实现，统计相同序对的个数
(define (filter predicate seq)
  (cond ((null? seq) 0)
        ((predicate (car seq))
         (+ 1 (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))


; For test

(define x (cons 'a 'b))
(define z1 (cons x x))
(count-pairs z1)
(count-pairs (cons (cons 1 2) (cons 3 4)))
