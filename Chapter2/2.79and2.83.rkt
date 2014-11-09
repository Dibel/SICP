#lang racket

(require racket/mpair)

(define assoc massoc)  ;Racke 中，可修改的列表是 mlist，而且相应的一些函数以 m 开头，比如 mcar ,mcdr, mcons, set-mcdr! 
(define set-cdr! set-mcdr!)  ;set-cdr!用衣修改 列表或者序对的cdr
;;; Generic Arithmetic

;;; Code for creating the table, you don't need to worry about this.

(define (make-table)
  (let ((local-table (mlist '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-cdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))




;;; The bottom level typing system

(define attach-tag cons)

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad typed datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad typed datum -- CONTENTS" datum)))


;;; The apply-generic mechanism.  
;;;  Note that we don't deal with coercion here.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (if (or (eq? op 'raise-poly) (null? (filter (lambda (x) (eq? x 'polynomial)) type-tags)))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (error "No method for the given types -- APPLY-GENERIC"
                 (mlist op type-tags))))
        (let ((var (cadr (car (filter (lambda (x) (eq? (type-tag x) 'polynomial)) args)))))
          (let ((proc (get op (map type-tag (map (raise-poly var) args)))))
           (if proc
              (apply proc (map contents (map (raise-poly var) args)))
              (error "No method for the given type -- APPLY-GENERIC"
                 (mlist op type-tags))))))
     ))


;;; Some generic arithmetic procedures

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;; The rational number package

(define (install-rational-package)
  ;; internal procedures
  (define numer car)
  (define denom cdr)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interfaces
  (define (tag x) (attach-tag 'rational x))
  (put 'equ '(rational rational)              ; equ?
       (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
  (put 'zero '(rational)                      ; =zero?
       (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational)                     ; raise
       (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
    
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your code goes here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; equ?

(define (equ? x y ) (apply-generic 'equ x y))

;; =zero?

(define (=zero? x) (apply-generic 'zero x))

;; raise

(define (raise x) (apply-generic 'raise x))

(define (negate x) (apply-generic 'negate x))

;; 自然数


(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'equ '(scheme-number scheme-number)  ; equ
       (lambda (x y) (= x y)))
  
  (put 'zero '(scheme-number)               ; =zero?
       (lambda (x) (= x 0)))
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))
  (put 'negate '(scheme-number)             ; 取负
       (lambda (x) (tag (- 0 x))))
  (put 'raise-poly '(scheme-number)
       (lambda (x) (lambda (var) (make-polynomial var (list (list 0 (make-scheme-number x)))))))
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;; 实数

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'make 'real
       (lambda (x) (tag x)))
  'done)

(define (make-real x)
  ((get 'make 'real) x))

;; 复数

;; Some essiential packages
(define (square x) (* x x))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;  Complex Package
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (put 'equ '(complex complex)                       ; equ?
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
                            (= (imag-part z1) (imag-part z2)))))
  (put 'zero '(complex)                              ; =zero?
       (lambda (x) (and (= (real-part x) 0) (= (imag-part x) 0))))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

; To provide all functions to 2.87and2.88and2.92
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (raise-poly var) (lambda (x) ((apply-generic 'raise-poly x) var)))

(provide (all-defined-out))

;;; Some basic testing

(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(display (equ? (make-complex-from-real-imag 1 0) (make-complex-from-mag-ang 1 0)))(newline)
(display (equ? (make-complex-from-real-imag 1 0) (make-complex-from-real-imag 1 1)))(newline)

(install-scheme-number-package)
(display (equ? (make-scheme-number 5) (make-scheme-number 5)))(newline)
(display (equ? (make-scheme-number 5) (make-scheme-number 6)))(newline)

(install-rational-package)
(display (equ? (make-rational 1 2) (make-rational 2 4)))(newline)
(display (equ? (make-rational 1 2) (make-rational 2 2)))(newline)
(install-real-package)

(display (raise (make-scheme-number 2)))(newline)
(display (raise (make-rational 1 2)))(newline)
(display (raise (make-real 2.1)))(newline)
(display "======2.83 TEST DONE======")(newline)