#lang racket/load

(load "drawing.rkt")

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
(define (flip-horiz painter)
  (transform-painter painter
             (make-vect 1.0 0.0)
             (make-vect 0.0 0.0)
             (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
             (make-vect 1.0 1.0)
             (make-vect 0.0 1.0)
             (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
             (make-vect 1.0 0.0)
             (make-vect 1.0 1.0)
             (make-vect 0.0 0.0)))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-up
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-down
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

(define (below painter1 painter2)
  (rotate270 (beside (rotate180 (rotate270 painter1)) (rotate180 (rotate270 painter2)))))

(define testFrame (make-frame (make-vect 10 10) (make-vect 299 0 ) (make-vect 0 299)))

(define (start-drawing) ((below wave2 wave) testFrame))