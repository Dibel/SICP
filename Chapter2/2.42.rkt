#lang scheme

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

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

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (define (iter i pos rest)
    (cond ((= i k) #t)
          ((or (= pos (car rest)) (= pos (- (car rest) i)) (= pos (+ (car rest) i))) #f)
          (else
           (iter (+ i 1) pos (cdr rest)))))
  (iter 1 (car positions) (cdr positions)))

(queens 6)