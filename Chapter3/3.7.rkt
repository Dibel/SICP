#lang scheme

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m)))
        (lambda (m) (display "Incorrect password")(newline))))
  dispatch)

(define (make-joint account old-pass new-pass)
  (lambda (p m)
    (if (eq? p new-pass)
        (account old-pass m)
        (lambda (m) (display "Incorrect password")(newline)))))


; For test

(define acc (make-account 100 'abc))

((acc 'abcd 'withdraw) 40)

(define new (make-joint acc 'abc 'bcd))

((new 'bcd 'withdraw) 40)
