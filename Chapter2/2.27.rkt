#lang scheme

(define (deep-reverse tree)
  (define (iter tree1 tree2)
    (cond ((null? tree1) tree2)
          ((not (pair? (car tree1)))
                (iter (cdr tree1) (cons (car tree1) tree2)))
          (else (iter (cdr tree1) (cons (iter (car tree1) '()) tree2)))))
  (iter tree '()))

(define (deep-reverse1 items)
  (if (pair? items)
      (reverse (map deep-reverse1 items))
      items))


(define (reverse items)
  (define (iter list1 list2)
    (if (null? list1)
        list2
        (iter (cdr list1) (cons (car list1)  list2))))
  (iter items '()))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

; For test
(deep-reverse (list 1 2 3 4 5))

(deep-reverse (list (list 1 2) (list 3 4) (list 4 5 6)))

(deep-reverse1 (list (list 1 2) (list 3 4) (list 4 5 6)))