; Exercise 2.3
;
; Implement a representation for rectangles in a plane.
; (Hint: You may want to make use of Exercise 2.2.)
;
; In terms of your constructors and selectors, create procedures that compute
; the perimeter and the area of a given rectangle.
;
; Now implement a different representation for rectangle.
;
; Can you design your system with suitable abstraction barriers, so that the
; same perimeter and area procedures will work using either representation?

#lang sicp

(define (average . xs)
  (define (iterate xs
                   counter
                   total)
    (if (null? xs)
        (/ total counter)
        (iterate (cdr xs)
                 (inc counter)
                 (+ total
                    (car xs)))))
  (iterate xs 0 0.0))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-rect p1 p2)
  (cons p1 p2))
(define (first-point-rect r)
  (car r))
(define (second-point-rect r)
  (cdr r))
(define (width-rect r)
  (let ((p1 (first-point-rect r))
        (p2 (second-point-rect r)))
    (abs (- (x-point p1)
            (x-point p2)))))
(define (height-rect r)
  (let ((p1 (car r))
        (p2 (cdr r)))
    (abs (- (y-point p1)
            (y-point p2)))))

(define r (make-rect (make-point 1 1)
                     (make-point 3 4)))

; (define (make-rect p w h)
;   (cons p (cons w h)))
; (define (width-rect r)
;   (car (cdr r)))
; (define (height-rect r)
;   (cdr (cdr r)))
; 
; (define r (make-rect (make-point 1 1)
;                      2
;                      3))


(define (perimeter-rect r)
  (* 2
     (+ (width-rect r)
        (height-rect r))))
(define (area-rect r)
  (* (width-rect r)
     (height-rect r)))

(width-rect r)
(height-rect r)
(perimeter-rect r)
(area-rect r)

