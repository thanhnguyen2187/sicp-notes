#lang sicp

(define (cube-root x)
  (define (good-enough? prev-guess next-guess)
    (< (abs (- prev-guess next-guess)) 0.0001))
  (define (improve y x)
    (/ (+ (/ x (* y y)) (* 2 y))
       3))
  (define (iter guess x)
    (if (good-enough? guess (improve guess x))
        guess
        (iter (improve guess x) x)))
  (iter 1.0 x))

(cube-root 3)
(cube-root 4)
(cube-root 8)
