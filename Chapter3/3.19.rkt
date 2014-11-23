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
  (let ((one-step 'one)
        (two-step 'two))
    (define (iter x y)
      (cond ((or (null? x) (null? y)) #f)
            ((eq? one-step two-step) #t)
            (else
             (set! one-step (cdr x))
             (if (pair? (cdr y))
                 (set! two-step (cdr (cdr y)))
                 (set! two-step '()))
             (iter one-step two-step))))
    (iter x x)))

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))


; For test
(define test (cons 'a (cons 'b (cons 'c '()))))
(set-cdr! (last-pair test) test)
(have-cycle? test)
