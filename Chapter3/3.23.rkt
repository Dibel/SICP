#lang scheme
(require scheme/mpair)

(define cdr mcdr)
(define car mcar)
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)
(define cons mcons)
(define list mlist)
(define pair? mpair?)
(define list? mlist?)

; 节点设计：(data . (backward-ptr . forward-ptr))
; car部分存数据，cdr部分为一个序对，该序对的car为上一个项的指针，cdr为下一个项的指针
; front的上一项指针为空，rear部分的下一项指针为空（保证正常删除）
; 其他部分与单队列基本相同

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (or (null? (front-ptr deque)) (null? (rear-ptr deque))))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty dueue" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty queue" deque)
      (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-cdr! (cdr new-pair) (front-ptr deque)) ; cdr->forward
           (set-car! (cdr (front-ptr deque)) new-pair) ; car->backward
           (set-front-ptr! deque new-pair)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-car! (cdr new-pair) (rear-ptr deque))
           (set-cdr! (cdr (rear-ptr deque)) new-pair)
           (set-rear-ptr! deque new-pair)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (cdr (cdr (front-ptr deque))))
         (if (null? (front-ptr deque))
             (set-rear-ptr! deque '()) ; 保证正常删除为空队列
             (set-car! (cdr (front-ptr deque)) '()))
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        (else
         (set-rear-ptr! deque (car (cdr (rear-ptr deque))))
         (if (null? (rear-ptr deque))
             (set-front-ptr! deque '()) ; 保证正常删除为空队列
             (set-cdr! (cdr (rear-ptr deque)) '()))
         deque)))

; For test

(define d (make-deque))
(empty-deque? d)
(display d)(newline)
(front-insert-deque! d '1)
(front-insert-deque! d '2)
(rear-insert-deque! d '3)
(rear-insert-deque! d '4)
(front-deque d)
(rear-deque d)
(front-delete-deque! d)
(front-deque d)
(rear-delete-deque! d)
(rear-deque d)
(rear-delete-deque! d)
(front-delete-deque! d)