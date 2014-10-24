
(require scheme/class scheme/gui/base)

(define (flatlist  lst); lst的每个元素是表， flatmap把每个元素里的元素抽取出来并成一张表
  (if (null? lst)
      (list )
      (append (car lst) (flatlist (cdr lst)))))

(define width 800)
(define height 600)
(define frame (new frame% [label ""]
                          [width width]
                          [height height]))
(define canvas (new canvas% [parent frame]
                            [paint-callback (lambda (canvas dc) (start-drawing))])) ;窗口刷新就会调用 start-drawing
;绘图的代码都要写在 start-drawing里面。the param of lambda ,canvas and dc are not used in start-drawing,because in start-drawing
;it uses the global canvas and dc

(define dc (send canvas get-dc))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define red-pen (make-object pen% "RED" 1 'solid))
(send dc set-pen red-pen)
(send dc set-brush no-brush)
(send frame show #t)
; customized draw-line and painter generator
(define (draw-line p1 p2)
  (send dc
        draw-line
        (xcor-vect p1) (ycor-vect p1) (xcor-vect p2) (ycor-vect p2)))


;for vectors. in fact ,vector is just point
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;for frames
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))  ;in fact ,all elements are points. edge1 and edge2's coordinate is based on origin
;for example, origin is (10,10) , edge1 is ( 0 ,100), then the corner decided by edge1 is (10,110)
;edge2 is (100,0),then,the other corner decided by edge2 is (110,10)

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (frame-coord-map frame);this is important. It's a function taking vector as param. 
  ;it map the vector into frame. usage((fram-coord-map frame ) v) . the coordinate of v must in range of [0,1]
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;for segments
(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))


(define (end-segment seg)
  (cdr seg))

(define (make-segment-from-pts x1 y1 x2 y2)
  (make-segment (make-vect x1 y1) (make-vect x2 y2)))

(define (make-segments-list lst) ;lst is a list of vectors. this function makes a series of segments connecting the vectors
  (if (null? (cdr lst))
      (list )
      (cons (make-segment (car lst) (cadr lst)) 
            (make-segments-list (cdr lst)))))

(define (MyDraw-line p1 p2);customized draw-line
  (send dc
        draw-line
        (xcor-vect p1) (ycor-vect p1) (xcor-vect p2) (ycor-vect p2)))


;painter function
(define (segments->painter segment-list);the return value is a function(painter) taking a frame as param.
  ;the function draws segments of segment-list inside the frame param
  ;all coordinates of segments must inside (0,0)-(1,1)
  (lambda (frame)
    (for-each 
     (lambda (segment)
       (MyDraw-line 
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


; ----------painters----------


; The painter that draws an ''X'' by connecting opposite corners of the frame.
(define (draw-x ) ; a painter function taking a frame as param 
  (lambda (frame)
   ((segments->painter
    (list
      (make-segment-from-pts 0 0 1 1)
      (make-segment-from-pts 1 0 0 1)))
   frame)))
; ----------operations----------
(define (transform-painter painter origin corner1 corner2); 以一个 frame为参数，把  (origin corner1 corner2)这个frame
 ;映射到 frame 中去 。它从painter 生成一个新 painter，叫 newpainter  如果 painter f 在 f上绘图
  ; newpainter f 就在 (origin corner1 corner2) 对应到f中的那部分,假设叫 f' 上绘图 。这里要求painter要画的东西都是在 (0,0 )-(1,1)范围内的
  ; (origin corner1 corner2)必须是 在((0,0) ( 0 1) ( 1 0))这个单位frame范围内的一个frame, 它和单位frame之间的关系，
  ;就是 f' 和 f的关系
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


(define (beside painter1 painter2) ;生成一个painter，以 frame 为参数,左半边画 painter1, 右半边画painter2
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define (below bottom-painter top-painter)
  (lambda (frame)
    ((transform-painter
       top-painter
       (make-vect 0 0)
       (make-vect 1 0)
       (make-vect 0 0.5))
     frame)
    ((transform-painter
       bottom-painter
       (make-vect 0 0.5)
       (make-vect 1 0.5)
       (make-vect 0 1))
     frame)))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (identity painter)
  painter)
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (up-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split2 painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split (rotate180 painter) n))))


; The wave painter. 
  (define (myscale vect-list)
    (define (reverse-y vct)
      (make-vect (xcor-vect vct) (- 1.0 (ycor-vect vct))))
    (map (lambda (vct) (reverse-y (scale-vect 0.024  vct))) vect-list))

(define waveShape
  (let ((v1 (list (make-vect 0 26)
                  (make-vect 6 17)
                  (make-vect 12 25)
                  (make-vect 14 21)
                  (make-vect 10 0)))
        (v2 (list (make-vect 16 0)
                  (make-vect 21 13)
                  (make-vect 25 0)))
        (v3 (list (make-vect 31 0)
                  (make-vect 25 19)
                  (make-vect 41 6)))
        (v4 (list (make-vect 41 15)
                  (make-vect 31 27)
                  (make-vect 25 27)
                  (make-vect 27 35)
                  (make-vect 25 41)))
        (v5 (list (make-vect 16 41)
                  (make-vect 14 35)
                  (make-vect 16 27)
                  (make-vect 12 27)
                  (make-vect 6 25)
                  (make-vect 0 35))))
  (flatlist (list (make-segments-list (myscale v1))
        (make-segments-list (myscale v2))
        (make-segments-list (myscale v3))
        (make-segments-list (myscale v4))
        (make-segments-list (myscale v5))))))

(define (wave frame) ;wave是一个painter，用 frame作为参数
  ((segments->painter waveShape ) frame))


(define oddShape-v 
         (list (make-vect 0 0)
               (make-vect 0.2 0.1)
               (make-vect 0.5 0.5)
               (make-vect 0.3 1)
               (make-vect 0 0)))


(define oddShape (make-segments-list oddShape-v))

(define (oddShape-painter frame)
  ((segments->painter oddShape ) frame))


(define wave2
  (beside wave (flip-vert wave)))

;(define wave4
 ; (below wave2 wave2))

(define (flip-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;(define wave4 (flip-pairs wave))
(define wave4 (corner-split wave 1))
;test
(define testFrame (make-frame (make-vect 10 10) (make-vect 299 0 ) (make-vect 0 299)))

;(define testFrame2 (make-frame (make-vect 10 99) (make-vect 10 10) (make-vect 99 99)))
;(define (start-drawing)  (wave testFrame))


