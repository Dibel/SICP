#lang scheme

; 原理解释
; 书上所讲的死锁情况是由于在并行过程中，两个账户的串行化进程分别被保护导致的
; 根据编号顺序进行交换的时候，总会先尝试保护编号较小的账户的串行化进程，那么即使
; 另一个操作员同时尝试进行交换操作，也会因为无法申请到小编号账户的串行化进程而等
; 待，从而避免了死锁

; My code begins at Line 50

(require scheme/mpair)

(define cdr mcdr)
(define car mcar)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)
(define cons mcons)
(define list mlist)
(define pair? mpair?)
(define list? mlist?)

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)
                 (void))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

; My code
(define make-serial
  (let ((serial 0))
    (lambda ()
      (set! serial (+ serial 1))
      serial)))

(define (make-account-and-serializer balance)
  (let ((serial (make-serial)))
    (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serial) serial)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange a1 a2)
  (define (iter account1 account2)
    (let ((serializer1 (account1 'serializer)))
      (let ((serializer2 (account2 'serializer)))
        ((serializer1 (serializer2 exchange))
         account1
         account2))))
  (if (< (a1 'serial) (a2 'serial))
      (iter a1 a2)
      (iter a2 a1)))

; For test
(define x (make-account-and-serializer 100))
(define y (make-account-and-serializer 200))
(y 'serial)
(x 'serial)
(serialized-exchange x y)
(y 'balance)
(x 'balance)