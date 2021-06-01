#lang sicp

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; (new-if (= 2 3) 0 5) ; 5
; (new-if (= 1 1) 0 5) ; 0

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

#| (define (sqrt-iter guess x) |#
#|   (new-if (good-enough? guess x) |#
#|           guess |#
#|           (sqrt-iter (improve guess x) x))) |#
; it loops infinitely since `new-if` is a function, and Scheme uses
; applicative-order evaluation, a.k.a. the arguments of a procedure call are
; evaluated before the procedure itself

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt 8)
