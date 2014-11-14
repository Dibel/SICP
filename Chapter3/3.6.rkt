#lang scheme
(define random-init 11111111)

(define (rand-update x)
  (remainder (+ 65536 (* x 1024)) 1024000))

(define rand
  (let ((x random-init))
    (lambda (args)
      (cond ((eq? args 'generate)
             (set! x (rand-update x))
             x)
            ((eq? args 'reset)
             (lambda (new-value)
               (set! x new-value)))
            (else
             (error "Unkown Args --RAND"))))))

; For test

((rand 'reset) 1)
(rand 'generate)
(rand 'generate)
((rand 'reset) 1)
(rand 'generate)
(rand 'generate)
