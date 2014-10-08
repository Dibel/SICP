#lang scheme

; 单次Miller-Rabin检查：(miller-test n)
; 按照概率，完整的检测至少要检查n/2次，可以使用fast-miller-prime完成一次完整检测

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (sqrt-test (expmod base (/ exp 2) m)
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
; 检查1取模n的非平凡平方根
(define (sqrt-test a m)
  (let ((mod (remainder (square a) m)))
    (if (and (> a 1) (< a (- m 1)) (= mod 1))
        0
        mod)))

(define (square n)
  (* n n))

(define (miller-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-miller-prime? n)
  (fast-prime? n (/ n 2)))

(define (fast-prime? n times)
    (cond ((< times 1) true)
        ((miller-test n) (fast-prime? n (- times 1)))
        (else false)))
