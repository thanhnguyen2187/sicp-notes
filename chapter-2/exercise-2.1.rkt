; Exercise 2.1
;
; Define a better version of `make-rat` that handles both positive and negative
; arguments. `make-rat` should normalize the sign so that if the rational number
; is positive, both the numerator and the denominator are positive, and if the
; rational number is negative, only the numerator is negative.

#lang sicp

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (signed-g (lambda (g) (if (< d 0)
                                  (* -1 g)
                                  g))))
    (cons (/ n (signed-g g))
          (/ d (signed-g g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(print-rat (make-rat -1 -2))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define minus-one-half (make-rat -1 2))
(define minus-one-third (make-rat -1 3))

(print-rat (add-rat minus-one-half one-third))
(print-rat (mul-rat one-half minus-one-third))
(print-rat (add-rat one-third one-third))
