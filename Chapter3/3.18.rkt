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

(define (have-cycle? x)
  (let ((achieve '())
        (count 0))
    (define (iter item)
      (cond ((null? item) #f)
            ((> count 5) #t)
            (else
             (set! achieve (cons (cons '() x) achieve))
             (if (inlist x achieve)
                 (set! count (+ count 1))
                 (void))
             (iter (cdr item)))))
    (iter x)))

(define (inlist item lst)
  (> (filter (lambda (x) (eq? item (cdr x))) lst) 3))

; 修改的filter实现,统计相同序对的个数
(define (filter predicate seq)
  (cond ((null? seq) 0)
        ((predicate (car seq))
         (+ 1 (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

; For test
(define test (cons 'a (cons 'b (cons 'c '()))))
(set-cdr! (last-pair test) test)
(have-cycle? test)
