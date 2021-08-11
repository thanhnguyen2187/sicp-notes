; Exercise 2.34
;
; Evaluating a polynomial in /x/ at a given value of /x/ can be formulated as an
; accumulation. We evaluate the polynomial
;
; ...
;
; using a well-known algorithm called /Horner's rule/, which structures the
; computation as
;
; ...
;
; In other words,
; - we start with /a_n/,
; - multiply by /x/,
; - add /a_n-1/,
; - multiply by /x/,
; - and so on,
; until we reach /a_0/.
;
; Fill in the following template to produce a procedure that evaluates a
; polynomial using Horner's rule. Assume that the coefficients of the polynomial
; are arranged in a sequence, from /a_0/ through /a_n/.

#lang sicp

(define (square x) (* x x))
(define (power b n)
  (cond ((= n 1) b)
        ((even? n) (square (power b (/ n 2))))
        ((odd? n) (* b (square (power b (/ (- n 1) 2)))))
        (else 1)))

(define ? 0)
(define (accumulate operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator
                            initial
                            (cdr sequence)))))

(define (horner-eval x
                     coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x)
                   this-coeff))
              0
              coefficient-sequence))

(horner-eval 2
             (list 1 3 0 5 0 1))
; (power 2 4)
