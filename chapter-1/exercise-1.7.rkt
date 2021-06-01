; The `good-enough?` test used in computing square roots will not be very
; effective for finding the square roots of very small numbers. Also, in real
; computers, arithmetic operations are almost always performed with limited
; precision. This makes our test inadequate for very large numbers.
;
; Explain these statements, with examples showing how the test fails for small
; and large numbers.
;
; An alternative strategy for implementing `good-enough?` is to watch how
; `guess` changes from one iteration to the next and to stop when the change is
; a very small fraction of the guess.
;
; Design a `square-root` procedure that uses this kind of end test.
;
; Does this work better for small and large numbers?

#lang sicp

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x) (* x x))

#| (define (good-enough? guess x) |#
#|   (< (abs (- (square guess) x)) 0.001)) |#

(define (good-enough? prev-guess next-guess)
  (< (abs (- prev-guess next-guess)) 0.0000001))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 1.00001) ; 1.0
(sqrt 9) ; 3.00000
(sqrt 810000000) ; 28460.49894151542
(square 28460.49894151542) ; 810000000.0000004
