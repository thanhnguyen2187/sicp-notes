; Exercise 1.46
;
; Several of the numerical methods described in this chapter are instances of an
; extremely general computational strategy known as /iterative improvement/.
;
; Iterative improvment stays that: to compute something,
; - we start with an initial guess for the answer,
; - test if the guess is good enough, and othewise
; - improve the guess and continue the process using the improved guess as the
; new guess.
;
; Write a procedure `iterative-improve` that takes two procedures as arguments:
; - a method for telling whether a guess is good enough and
; - a method for improving a guess.
;
; `iterative-improve` should return as its value a procedure that takes a guess
; as a argument and keeps improving the guess until it is good enough. Rewrite
; the `sqrt` procedure of Section 1.1.7 and the `fixed-point` procedure of
; Section 1.3.3 in terms of `iterative-improve`.

#lang sicp

;;
(define (iterative-improve good-enough?
                           improve)
  (define (iterate guess)
    (let ((next (improve guess)))
      (if [good-enough? next]
          next
          (iterate next))))
  (lambda (first-guess) (iterate first-guess)))

;;
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

;;
(define (square x) (* x x))
(define tolerance 0.00001)

;;
(define (sqrt-v2 x)
  (define (good-enough? guess)
    (< (abs (- (square guess)
               x))
       tolerance))
  (define (improve guess)
    (average guess
             (/ x
                guess)))
  ((iterative-improve good-enough?
                      improve) x))

;;
(define (fixed-point-v2 f
                        first-guess)

  (define (good-enough? guess)
    (let ((next (f guess)))
      (< (abs (- next
                 (f next)))
         tolerance)))

  ((iterative-improve good-enough?
                      f) first-guess))

; (define fixed-point-cos (fixed-point-v2 cos 1.0))
; (fixed-point-cos 2.0)

(sqrt-v2 4)
(fixed-point-v2 cos 1)
