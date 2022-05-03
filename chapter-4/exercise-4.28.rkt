; Exercise 4.28
;
; `eval` uses `actual-value` rather than `eval` to evaluate the operator before
; passing it to `apply`, in order to force the value of the operator. Give an
; example that demonstrates the need for this forcing.
;
; ---
;
; Without this forcing, a higher-order function is going to be a "thunk", which
; is inapplicable to anything else.

#lang sicp

(define add-3
  (lambda (x) (+ 3 x)))

(eval '(add-3 (+ 4 5)))

