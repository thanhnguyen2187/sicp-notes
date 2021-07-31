; Exercise 2.2
;
; Consider the problem of representing line segments in a plane. Each segment is
; represented as a pair of points: a starting point and ending point.
;
; Define - a constructor `make-segment` and - selectors `start-segment` and
; `end-segment` that define the representation of segments in terms of points.
;
; Furthermore, a point can be represented as a pair of numbers: - the /x/
; coordinate - the /y/ coordinate
;
; Accordingly, specify - a constructor `make-point` and - selectors `x-point`
; and `y-point` that define this representation.
;
; Finally, using your selectors and constructors, define a procedure
; `midpoint-segment` that takes a line segment as argument and returns its
; midpoint (the point whose coordinates are the average of the coordinates of
; the endpoints).
;
; To try your procedures, you'll need a way to print points:
;
; ...

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

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (make-point (average (x-point p1)
                         (x-point p2))
                (average (y-point p1)
                         (y-point p2)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define p1 (make-point 1 2))
(define p2 (make-point 3 4))
(define s (make-segment p1 p2))

(print-point (midpoint-segment s))
