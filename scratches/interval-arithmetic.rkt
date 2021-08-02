; Exercise ?.?

#lang sicp

; It is sometimes necessary for them to compute the value of a parallel
; equivalent resistance R_p of two resistors R_1, R_2 using the formula:
;
; R_p = 1 / (1/R_1 + 1/R_2)
;
; Resistance values are usually known only up to some tolerance guaranteed by
; the manufacturer of the resistor. For example, if you buy a resistor labeled
; "6.8 ohms with 10% tolerance" you can only be sure that the resistor has a
; resistance between 6.8 - 0.68 = 6.12 and 6.8 + 0.68 = 7.48 ohms.
;
; Thus if you have a 6.8-ohm 10% resistor in parallel with a 4.7-ohm 5%
; resistor, the resistance of the combination can rance from 2.58 ohms (if the
; two resistors are at the lower bounds) to about 2.97 ohms (if the two
; resistors are at the upper bounds).
;
; Alyssa's idea is to implement "interval arithmetic" as a set of arithmetic
; operations for combining "intervals" (objects that represent the range of
; possible values of an inexact quantity). The result of adding, substracting,
; multiplying, or dividing two intervals is itself an interval, representing by
; the range of the result.
;
; Alyssa postulates the existance of an abstract object called an "interval"
; that has two endpoints: a lower bound and an upper bound. She also presumes
; that, given the endpoints of an interval, she can construct the interval using
; the data constructor `make-interval`. Alyssa first writes a procedure for
; adding two intervals. She reasons that the minimum value the sum could be is
; the sum of the two lower bounds and the maximum value it could be is the sum
; of the two upper bounds:
;
; ...
;
; Alyssa also works out the product of two intervals by finding the minimum and
; the maximum of the prodcuts of the bounds and using them as the bounds of the
; resulting interval. (`min` and `max` are primitives that find the minimum or
; maximum of any number of arguments.)
;
; ...
;
; To divide two intervals, Alyssa multiplies the first by the reciprocal of the
; second. Note that the bounds of the reciprocal interval are the reciprocal of
; the upper bound and the reciprocal of the lower bound, in that order.
;
; ...

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

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

; (define i1 (make-interval -1 6))
; (define i2 (make-interval -2 5))
(define i1 (make-center-percent 12 0.025))
(define i2 (make-center-percent -10 0.03))

; (display i1)
; (display i2)
; 
; (define i1+i2 (add-interval i1 i2))
; (display i1+i2)
; (newline)
; (center i1+i2)
; (percent i1+i2)
; 
; (define i1*i2 (mul-interval i1 i2))
; (display i1*i2)
; (newline)
; (center i1)
; (center i2)
; (center i1*i2)
; (percent i1*i2)
(define one (make-interval 1 1))
(define 1/i1 (div-interval one i2))

(center 1/i1)
(percent 1/i1)
