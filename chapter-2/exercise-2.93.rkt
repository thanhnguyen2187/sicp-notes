; Exercise 2.93
;
; Modify the rational-arithmetic package to use generic operations, but change
; `make-rat` so that it does not attempt to reduce fractions to lowest terms.
; Test your system by calling `make-rational` on two polynomials to produce a
; rational function:
;
; ...
;
; Now add `rf` to itself. You will observe that this addition procedure does not
; reduce fractions to lowest terms.

#lang sicp

(define p1 (make-polynomial 'x) '((2 1) (0 1)))
(define p2 (make-polynomial 'x) '((3 1) (0 1)))
(define rf (make-rational p2 p1))

(add rf rf)
