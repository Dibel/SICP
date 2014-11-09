#lang racket

;; 2.87 At Line 124
;; 2.88 使用了通用求负过程negate，其中一些针对scheme-number的定义存在于2.79and2.83.rkt
;; 2.92 修改了apply-generic过程，对数（仅限scheme-number）提供了raise-poly以便与多项式进行计算。
;; 对多项式提供了raise操作，以便提升为x的多项式进行计算

(require "2.79and2.83.rkt")
;; My code
(define (install-polynomial-package)

  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (tag p) (attach-tag 'polynomial p))
  
  (define (raisepoly x) ;; 提升多项式至x
    (if (same-variable? (variable x) 'x)
        (void)
        (let ((seq-x (filter has-x? (term-list x))))
          (if (null? seq-x)
            (make-poly 'x (list (list 0 (tag x))))
            (let ((seq-var (filter has-var? (term-list x))))
              (if (null? seq-var)
                  (accumulate add empty-poly
                        (map (trans-x (variable x)) seq-x))
                  (accumulate add (tag (make-poly 'x (list (list 0 (tag (make-poly (variable x) seq-var))))))
                        (map (trans-x (variable x)) seq-x))))))))
  
  (define (trans-x var) ;; 变换多项式至x
    (lambda (item) (mul (coeff item) (tag (make-poly 'x (list (list 0 (tag (make-poly var (list (list (order item) (make-scheme-number 1))))))))))))
  (define empty-poly (tag (make-poly 'x '())))
  (define (has-x? item) ;; 多项式系数中是否存在x的多项式
    (if (and (pair? (contents (coeff item))) (same-variable? (variable (contents (coeff item))) 'x))
        #t
        #f))
  (define (has-var? item)
    (not (has-x? item)))
  (define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))
  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (sub-poly p1 p2)         ;; sub
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (negate-list (term-list p2))))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))
  (define (negate-list lst)
    (map (lambda (term) (list (order term) (negate (coeff term)))) lst))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (define (mul-term-by-all-terms t1 L)
      (if (empty-termlist? L)
          (the-empty-termlist)
          (let ((t2 (first-term L)))
            (adjoin-term
             (make-term (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
             (mul-term-by-all-terms t1 (rest-terms L))))))
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (put 'zero '(polynomial)                          ;;=zero?
     (lambda (x)
       (if (pair? x)
           #f
           (= x 0))))
  
  (put 'negate '(polynomial)                        ;; negate
       (lambda (x)
         (make-poly (variable x)
                          (map (lambda (term) (list (order term) (negate (coeff term)))) (term-list x)))))
  (put 'raise '(polynomial)                       ;; raise
       (lambda (x) (raisepoly x)))
  (put 'raise-poly '(polynomial)
       (lambda (x) (lambda (var) (tag x))))
  (put 'add '(scheme-number polynomial)
       (lambda (p1 p2)
         (tag (add-poly (raise-poly (make-scheme-number p1) (variable p2)) p2))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'mul '(scheme-number polynomial)
       (lambda (p1 p2)
         (tag (mul-poly (contents (raise-poly (make-scheme-number p1) (variable p2))) p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


;; For test

(newline)(display "======TEST BEGIN=====")(newline)
(install-scheme-number-package)
(install-polynomial-package)

(define x (make-scheme-number 1))
(define y (make-scheme-number 3))
(define p1 (make-polynomial 'x (list (list 100 x) (list 2 y) (list 1 x))))
(define p2 (make-polynomial 'x (list (list 99 x) (list 2 y) (list 1 x))))

(display (=zero? p1))(newline)
(display p1)(newline)
(display (add p1 p2))(newline)
(display (sub p1 p2))(newline)

(define p3 (make-polynomial 'y (list (list 100 x) (list 2 p1) (list 0 x))))
(display (raise p3))