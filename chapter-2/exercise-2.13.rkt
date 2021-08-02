; Exercise 2.13
;
; Show that under the assumption of small percentage tolerances, there is a
; simple formula for the approximate percentage tolerance of the product of two
; interval in terms of the tolerances of the factors.
;
; You may simplify the problem by assuming that all numbers are positive.

#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-width c w)
  (make-interval (- c w)
                 (+ c w)))
(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))
(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (make-center-percent c p)
  (make-interval (* c (- 1 p))
                 (* c (+ 1 p))))
(define (percent i)
  (/ (width i)
     (center i)))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; (define i1 (make-center-percent 6 0.5))
; (define i2 (make-center-percent 8 0.4))
(define i1 (make-center-percent 6 0.005))
(define i2 (make-center-percent 8 0.004))
(define i3 (mul-interval i1 i2))

(display i1)
(newline)
(display i2)
(newline)
(display i3)
(newline)

(percent i3)

; p3 = (p1 + p2) / (1 + p1*p2)
; p3 ~ (p1 + p2)
