; Exercise 1.45
;
; We saw in Section 1.3.3 that attempting to compute square roots by naively
; finding a fixed poitn of y |-> x/y does not converge, and that this can be
; fixed by average damping.
;
; The same method works for finding cube roots as fixed points of the
; average-damped y |-> x/y^2. Unfortunately, the process does not work for
; fourth roots -- a single average damp is not enough to make a fixed-point
; search for y |-> x/y^3 converge. On the other hand, if we average damp twice
; (i.e., use the average damp of the average damp of y |-> x/y^3) the fixed
; point search does converge.
;
; Do some experiments to determine how many average damps are required to
; compute /n^th/ roots as a fixed-point based upon repeated average damping of
; y|-> x/y^(n-1). Use this to implement a simple procedure for computing /n^th/
; roots using `fixed-point`, `average-damp`, and the `repeat` procedure of
; Exercise 1.43. Assume that any arithmetic operations you need are available as
; primitives.

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

(define (average-damp f)
  (lambda (x) (average x
                       (f x))))

(define (square x) (* x x))
(define (cube x) (* (square x)
                    x))

(define (nth-power x n)
  (cond ((= n 1) x)
        ((even? n) (square (nth-power x
                                      (/ n 2))))
        (else (* x
                 (square (nth-power x
                                    (/ (- n 1) 2)))))))

(define (repeat f
                times)
  (define (iterate x times)
    (if [= times 1]
        (f x)
        (iterate (f x)
                 (- times 1))))
  (lambda (x) (iterate x times)))

(define (sqrt-v2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(define (cube-root-v2 x)
  (fixed-point (average-damp (lambda (y) (/ x
                                            (square y))))
               1.0))

(define (nth-root n)
  (lambda (x)
    (fixed-point ((repeat average-damp
                          (if [> n 2]
                              (- n 1)
                              1))
                  (lambda (y)
                    (/ x
                       (nth-power y
                                  (- n
                                     1)))))
                 1.0)))

(define sqrt-v3 (nth-root 2))
(define cube-root-v3 (nth-root 3))
(define fourth-root (nth-root 4))
(define fifth-root (nth-root 5))

(sqrt-v3 9)
(cube-root-v3 8)
(fourth-root 16)
(fifth-root 32)

