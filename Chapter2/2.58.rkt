#lang scheme

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((sin? exp)
         (make-product (deriv (sin-num exp) var)
                       (make-cos (sin-num exp))))
        ((cos? exp)
         (make-product (deriv (cos-num exp) var)
                       (make-product '-1 (make-sin (cos-num exp)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

; 用来在表中搜索某一符号，使用了continuation
(define (linear-search wanted lst)
  (call/cc (lambda (return)
             (for-each (lambda (k)
                         (if (eq? k wanted)
                             (return #t)
                             (void)))
                       lst)
             #f)))

(define (variable? x) (symbol? x))

; 如果表中含有加号，就把整个式子看作加式
(define (sum? x)
  (and (pair? x) (linear-search '+ x)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (product? m1) (number? m2)) ; 以下均为简化:若两个乘数中有一个为数字且另一个乘数是一个含有数字的乘式则把这两个数字相乘以得到新的简化式
         (cond ((number? (multiplier m1))
                (make-product (* (multiplier m1) m2) (multiplicand m1)))
               ((number? (multiplicand m1))
                (make-product (multiplier m1) (* m2 (multiplicand m1))))))
        ((and (product? m2) (number? m1))
         (cond ((number? (multiplier m2))
                (make-product (* (multiplier m2) m1) (multiplicand m2)))
               ((number? (multiplicand m2))
                (make-product (multiplier m2) (* m1 (multiplicand m2))))))
        (else (list m1 '* m2))))
(define (=number? a1 a2)
  (and (number? a1) (= a1 a2)))

(define (addend exp) (car exp))
(define (augend exp)
  (if (null? (cdddr exp))
      (caddr exp)
      (cddr exp)))

(define (multiplier exp) (car exp))
(define (multiplicand exp) 
  (if (null? (cdddr exp))
      (caddr exp)
      (cddr exp)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))


(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? e) (number? b))
         (fast-expt b e))
        ((exponentiation? b) ; 合并乘方
         (make-exponentiation (base b) (make-product (exponent b) e)))
        (else (list b '** e))))

(define (make-sub s1 s2)
  (cond ((=number? s1 0) (- 0 s2))
        ((=number? s2 0) s1)
        ((and (number? s1) (number? s2)) (- s1 s2))
        ((same-variable? s1 s2) 0)
        (else (list '- s1 s2))))

(define (fast-expt b n)
  (fastexpt-iter b n 1))

(define (fastexpt-iter b n a)
  (if (= n 0)
      a
      (if (even? n)
          (fastexpt-iter (square b) (/ n 2) a)
          (fastexpt-iter b (- n 1) (* b a)))))

(define (square n)
  (* n n))

; sin
(define (sin? x)
  (and (pair? x) (eq? (car x) 'sin)))
(define (sin-num exp) (cadr exp))
(define (make-cos x)
  (list 'cos x))

;cos
(define (cos? x)
  (and (pair? x) (eq? (car x) 'cos)))
(define (cos-num exp) (cadr exp))
(define (make-sin x)
  (list 'sin x))

;ln
(define (ln? x)
  (and (pair? x) (eq? (car x) 'ln)))
(define (ln-num exp) (cadr exp))
(define (make-frac x)
  (if (number? x)
      (/ 1 x)
      (list '/ 1 x)))


; For test
(deriv '(x + y * (x + 2) * 5) 'x)

(deriv '((y * x) + (cos x) * 12 * x) 'x)