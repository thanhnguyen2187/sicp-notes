; Exercise 2.47
;
; Here are two possible constructors for frames:
;
; ...
;
; For each constructor supply the appropiate selectors to produce an
; implementation for frames.

#lang sicp

(define (make-frame origin
                    edge1
                    edge2)
  (list origin edge1 edge2))

(define (make-frame-v2 origin
                       edge1
                       edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (origin-frame-v2 frame)
  (car frame))
(define (edge1-frame-v2 frame)
  (cadr frame))
(define (edge2-frame-v2 frame)
  (cddr frame))

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

(define (scale-vect v n)
  (make-vect (* (xcor-vect v) n)
             (* (ycor-vect v) n)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define L (list 1 2 3 4 5))
(define L2 (cons 1 (cons 2 3)))

(car L2)
(cadr L2)
(cddr L2)
