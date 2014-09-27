;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |1.11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
; 1.11 递归
(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1)) (* (f1 (- n 2)) 2) (* (f1 (- n 3)) 3))))

; 1.11 迭代
(define (f2 n)
  (if (< n 3)
      n
      (f2-iter 2 1 0 n)))
(define (f2-iter a b c n)
  (if (< n 3)
      a
      (f2-iter (+ a (* b 2) (* c 3)) a b (- n 1))))