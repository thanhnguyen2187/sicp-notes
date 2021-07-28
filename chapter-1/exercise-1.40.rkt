; Exercise 1.40
;
; Define a procedure `cubic` that can be used together with the `newtons-method`
; procedure in expressions of the form
;
; ...
;
; to approximate zeroes of the cubic /x^3 + ax^2 + bx + c/.

#lang sicp

(define tolerance 0.00001)
(define (fixed-point f
                     first-guess)
  (define (close-enough? value-1 value-2)
    (< (abs (- value-1
               value-2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if [close-enough? guess next]
          next
          (try next))))
  (try first-guess))

(define (newton-transform g)
  (lambda (x) (- x
                 (/ (g x)))))
(define (newtons-method g
                        guess)
  (fixed-point (newton-transform g)
               guess))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (cubic a
               b
               c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* (+ b 1) x)
                 (* c))))

(newtons-method (cubic 1
                       2
                       3)
                1.0)
