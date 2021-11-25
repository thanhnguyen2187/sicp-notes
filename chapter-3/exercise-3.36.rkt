; Exercise 3.36
;
; Suppose we evaluate the following sequence of expressions in the global
; environment:
;
; ...
;
; At some time during evaluation of the `set-value!`, the following expression
; from the connector's local procedure is evaluated:
;
; ...
;
; Draw an environment diagram showing the environment in which the above
; expression is evaluated.
;
; Global:
; - make-connector
; - for-each-except
; - a
;   - value
;   - informant
;   - constraints
;   - set-my-value
;       - newval
;       - setter
;       - for-each-except
; - b
;
; So when `set-value!` of `a` is evaluated, `for-each-except` is also evaluated.

#lang sicp

(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

(for-each-except
  setter inform-about-value constraints)

