; Exercise 2.49
;
; Use `segments->painter` to define the following primitive painters:
;
; a. The painter that draws the outline of the designated frame.
; b. The painter that draws an "X" by connecting oposite corners of the frame.
; c. The painter that draws a diamond shape by connecting the midpoints of the
; sides of the frame.
; d. The `wave` painter.

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
    (add-vect
     (origin-frame frame)
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

(define frame-1-1 (make-frame (make-vect 0   0  )
                              (make-vect 0   500)
                              (make-vect 500 0  )))
(define painter-a
  (segments->painter
    (list (make-segment (make-vect 0.0 0.0)
                        (make-vect 1.0 0.0))
          (make-segment (make-vect 1.0 0.0)
                        (make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 1.0)
                        (make-vect 0.0 1.0))
          (make-segment (make-vect 0.0 1.0)
                        (make-vect 0.0 0.0)))))

(define painter-b
  (segments->painter
    (list (make-segment (make-vect 0.0 0.0)
                        (make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 0.0)
                        (make-vect 0.0 1.0)))))

(define painter-c
  (segments->painter
    (list (make-segment (make-vect 0.5 0.0)
                        (make-vect 1.0 0.5))
          (make-segment (make-vect 1.0 0.5)
                        (make-vect 0.5 1.0))
          (make-segment (make-vect 0.5 1.0)
                        (make-vect 0.0 0.5))
          (make-segment (make-vect 0.0 0.5)
                        (make-vect 0.5 0.0))
          )))

(define (accumulate operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator
                            initial
                            (cdr sequence)))))

(define (accumulate-n operator
                      initial
                      sequences)
  (if (null? (car sequences))
      '()
      (cons (accumulate operator
                        initial
                        ; accumulate the first elements of the sequences
                        (map car sequences))
            (accumulate-n operator
                          initial
                          (map cdr sequences)))))

(define (flatmap procedure
                 sequence)
  (accumulate append
              '()
              (map procedure
                   sequence)))

(define (square x) (* x x ))
(define (average a b)
  (/ (+ a b) 2))
(define (distance v1 v2)
  (sqrt (+ (square (- (xcor-vect v1)
                      (xcor-vect v2)))
           (square (- (ycor-vect v1)
                      (ycor-vect v2))))))
(define (middle-line v1 v2)
  (make-vect (average (xcor-vect v1) (xcor-vect v2))
             (average (ycor-vect v1) (ycor-vect v2))))

(define tolerance 0.01)
(define (smooth v1 v2 v3)
  ; smooth by averages until the distance between v2 and middle of v1 and v3 is
  ; less than tolerance
  (let ((middle-v1-v3 (middle-line v1 v3)))
    (if (< (distance middle-v1-v3 v2)
           tolerance)
        (list v1 v2 v3)
        (flatmap (lambda (vectors)
                   (let
                     ((v1 (car vectors))
                      (v2 (cadr vectors))
                      (v3 (caddr vectors)))
                     (smooth v1 v2 v3)))
                 (list (list v1
                             (middle-line v1 v2)
                             (middle-line middle-v1-v3 v2))
                       (list (middle-line middle-v1-v3 v2)
                             (middle-line v3 v2)
                             v3))))))

(define (zip sequence-1
             sequence-2)
  (if (or (null? sequence-1)
          (null? sequence-2))
      '()
      (cons (list (car sequence-1)
                  (car sequence-2))
            (zip (cdr sequence-1)
                 (cdr sequence-2)))))

(define (make-segments vectors)
  (map (lambda (pair) (make-segment (car pair)
                                    (cadr pair)))
       (zip vectors
            (cdr vectors))))

; (painter-a frame-1-1)
; (painter-b frame-1-1)
; (painter-c frame-1-1)

(define v1 (make-vect 0.0 0.5))
(define v2 (make-vect 1.0 1.0))
(define v3 (make-vect 0.2 0.7))

(define raw-segments (list (make-segment v1 v2)
                           (make-segment v2 v3)))

(define smoothed-vectors (smooth v1 v2 v3))
(define smoothed-segments (make-segments smoothed-vectors))

; (print smoothed-vectors)
; (print smoothed-segments)

(define painter-smoothed (segments->painter smoothed-segments))
(define painter-raw (segments->painter raw-segments))

(painter-smoothed frame-1-1)
; (painter-raw frame-1-1)

; (zip (list 1 2 3 4) (list 2 3 4))
