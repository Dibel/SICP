#lang scheme

(define (same-parity1 x . z)
  (define (iter list-a list-b)
    (if (null? list-a)
        list-b
        (let ((value (car list-a)))
          (if(even? (- value x))
             (iter (cdr list-a) (append list-b (list value)))
             (iter (cdr list-a) list-b)))))
  (iter z (list x)))

(define (same-parity x . z)
  (define (iter list1)
    (cond ((null? list1) '())
          ((even? (- (car list1) x))
           (cons (car list1) (iter (cdr list1))))
          (else (iter (cdr list1)))))
  (cons x (iter z)))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)

(same-parity 8 3 4 5 6 7)