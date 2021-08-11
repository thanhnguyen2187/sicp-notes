; Exercise 2.50

#lang racket

(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

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

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

(define (make-segment v1 v2)
  (list v1 v2))

(define (start-segment s)
  (car s))
(define (end-segment s)
  (cadr s))

;;

(define (make-frame origin
                    edge1
                    edge2)
  (list origin
        edge1
        edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (edge1-frame frame)
                                    (xcor-vect v))
                        (scale-vect (edge2-frame frame)
                                    (ycor-vect v))))))

(define (vector-to-posn v)
  (make-posn (ycor-vect v)
             (xcor-vect v)))

(define (segments->painter segment-list)   
  (lambda (frame)
    (for-each
      (lambda (segment)
        (line
          (vector-to-posn ((frame-coord-map frame) (start-segment segment)))
          (vector-to-posn ((frame-coord-map frame) (end-segment segment)))))
      segment-list)))

(define (transform-painter painter
                           origin
                           corner1
                           corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define frame-1-1 (make-frame (make-vect 0   0  )
                              (make-vect 0   500)
                              (make-vect 500 0  )))

(define painter-placeholder
  (segments->painter
    (list (make-segment (make-vect 0.0 0.0)
                        (make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 0.0)
                        (make-vect 0.0 1.0))
          (make-segment (make-vect 0.0 0.0)
                        (make-vect 1.0 0.0))
          (make-segment (make-vect 1.0 0.0)
                        (make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 1.0)
                        (make-vect 0.0 1.0))
          (make-segment (make-vect 0.0 1.0)
                        (make-vect 0.0 0.0))
          (make-segment (make-vect 0.25 0.0)
                        (make-vect 0.75 1.0))
          )))
(define painter-line
  (segments->painter
    (list (make-segment (make-vect 0.0 0.1)
                        (make-vect 0.2 0.3)))))

(painter-line frame-1-1)
; (painter-placeholder frame-1-1)
