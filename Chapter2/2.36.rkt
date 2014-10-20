#lang scheme

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (filter car seqs))
            (accumulate-n op init (filter cdr seqs)))))

; 一个抽象的、用于过滤序列的序列的过程
(define (filter op seqs)
  (cond ((null? seqs) '())
        ((not (pair? (car seqs)))
         (op seqs))
        (else (cons (filter op (car seqs)) (filter op (cdr seqs))))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(filter cdr (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))