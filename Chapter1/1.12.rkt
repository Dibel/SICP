#lang scheme

;用法
;(pascal 行数 列数)
;行数、列数均从1开始

(define (pascal row col)
  (cond ((= col 1) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))