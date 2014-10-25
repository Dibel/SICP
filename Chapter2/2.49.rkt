#lang racket/load

(load "drawing.rkt")


(define v1 (make-vect 0.0 0.0))

(define v2 (make-vect 0.0 1.0))

(define v3 (make-vect 1.0 0.0))

(define v4 (make-vect 1.0 1.0))

(define v5 (make-vect 0.0 0.5))

(define v6 (make-vect 0.5 0.0))

(define v7 (make-vect 1.0 0.5))

(define v8 (make-vect 0.5 1.0))

; 边界
(define line (list (make-segment v1 v2) (make-segment v1 v3) (make-segment v2 v4) (make-segment v3 v4)))
(define line-painter (segments->painter line))

; 叉子
(define fork (list (make-segment v1 v4) (make-segment v2 v3)))
(define fork-painter (segments->painter fork))

; 菱形
(define rhombus (list (make-segment v5 v6) (make-segment v6 v7) (make-segment v7 v8) (make-segment v8 v5)))
(define rhombus-painter (segments->painter rhombus))

; wave

(define wave (list
                         (make-segment (make-vect 0.4 0.0) 
                                       (make-vect 0.35 0.15))
                         (make-segment (make-vect 0.35 0.15)    
                                       (make-vect 0.4 0.36))
                         (make-segment (make-vect 0.4 0.36)  
                                       (make-vect 0.25 0.35))
                         (make-segment (make-vect 0.25 0.35)   
                                       (make-vect 0.15 0.4))
                         (make-segment (make-vect 0.15 0.4)    
                                       (make-vect 0.0 0.15))

                         (make-segment (make-vect 0.0 0.35)     
                                       (make-vect 0.15 0.65))
                         (make-segment (make-vect 0.15 0.65)  
                                       (make-vect 0.25 0.4))

                         (make-segment (make-vect 0.25 0.4)   
                                       (make-vect 0.35 0.5))
                         (make-segment (make-vect 0.35 0.5)   
                                       (make-vect 0.25 1.0))
                         (make-segment (make-vect 0.6 0.0)     
                                       (make-vect 0.65 0.15))
                         (make-segment (make-vect 0.65 0.15)    
                                       (make-vect 0.6 0.35))
                         (make-segment (make-vect 0.6 0.35)   
                                       (make-vect 0.75 0.35))
                         (make-segment (make-vect 0.75 0.35)    
                                       (make-vect 1.0 0.7))

                         (make-segment (make-vect 1.0 0.85)     
                                       (make-vect 0.6 0.5))
                         (make-segment (make-vect 0.6 0.5)
                                       (make-vect 0.75 1.0))

                         (make-segment (make-vect 0.4 1.0)
                                       (make-vect 0.5 0.7))
                         (make-segment (make-vect 0.6 1.0)
                                       (make-vect 0.5 0.7))))

(define wave-painter (segments->painter wave))

(define testFrame (make-frame (make-vect 10 10) (make-vect 299 0 ) (make-vect 0 299)))

(define (start-drawing)  (wave-painter testFrame))
