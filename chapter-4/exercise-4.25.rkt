; Exercise 4.25
;
; Suppose that (in ordinary applicative-order Scheme) we define `unless` as
; shown above and then define `factorial` in terms of `unless` as
;
; ...
;
; What happens if we attempt to evaluate `(factorial 5)`? Will our definitions
; work in a normal-order language?
;
; ---
;
; If we attempt to evaluate `(factorial 5)`, `(factorial 4)` gets evaluated even
; if `(= 4 1)` is `false`. Our definition then does work in a normal-order
; language, since `(= n 1)` is going to be checked before `(factorial (- n 1))`.

#lang sicp

(define (unless
          condition
          usual-value
          exceptional-value)
  (if condition
    exceptional-value
    usual-value))

(define (factorial n)
  (unless
    (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 5)

(unless (= 5 1)
  (* 5 (factorial 4 1))
  1)

(unless (= 5 1)
  (* 5 (unless (= 4 1)
         (* 4 (factorial 3 1))
         1))
  1)
