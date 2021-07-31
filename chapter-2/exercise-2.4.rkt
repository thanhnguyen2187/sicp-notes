; Exercise 2.4
;
; Here is an alternative procedural representation of pairs. For this
; representation, verify that `(car (cons x y))` yield `x` for any object `x`
; and `y`.
;
; ...
;
; What is the corresponding definition of `cdr`? (Hint: to verify that this
; works, make use of the substitution model of Section 1.1.5.)

#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(define pair (cons 3 4))
; (lambda (m) (m 3 4))
(car pair)
; ((lambda (m) (m 3 4)) (lambda (p q) q))
; ((lambda (p q) q) 3 4)
; 4
(cdr pair)
