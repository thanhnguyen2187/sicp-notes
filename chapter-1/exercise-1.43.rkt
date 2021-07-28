; Exercise 1.43
;
; If /f/ is a numerical function and /n/ is a positive integer, then we can form
; the /n^th/ repeated application of /f/, which is defined to be the function
; whose value at /x/ is f(f(...(f(x)))).
;
; For example, if /f/ is the function /x |-> x + 1/, then the /n^th/ repeated
; application of /f/ is the function /x |-> x + n/. If /f/ is the operation of
; squaring a number, then the /n^th/ repeated application of /f/ is the function
; that raises its argument to the /2^n/-th power.
;
; Write a procedure that takes as inputs a procedure that computes /f/ and a
; positive integer /n/ and returns the procedure that computes the /n^th/
; repeated application of /f/. Your procedure should be able to be used as
; follows:
;
; ...

#lang sicp

(define (repeated f n)
  (if [= n 1]
      f
      (repeated (lambda (x) (f (f x)))
                (- n 1))))

((repeated inc 5) 1)
