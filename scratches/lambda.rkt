;

#lang sicp

(define (square x) (* x x))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
; Immediately Invoked Function Expression
