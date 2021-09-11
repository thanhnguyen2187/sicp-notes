; Exercise 3.10
;
; In the `make-withdraw` procedure, the local variable `balance` is created as a
; parameter of `make-withdraw`. We could also create the local state variable
; explicitly, using `let`, as follows:
;
; ...
;
; Recall from Section 1.3.2 that `let` is simply syntactic sugar for a procedure
; call:
;
; ...
;
; is interpreted as an alternate syntax for:
;
; ...
;
; Use the environment model to analyze this alternate version of
; `make-withdraw`, drawing figures like the ones above to illustrate the
; interactions:
;
; ...
;
; Show that the two version of `make-withdraw` create objects with the same
; behavior. How do the environment structures differ for the two versions?
;
; ---
;
; The second version of `make-withdraw` create a variable named `balance` that
; acts the same as `balance` within the first `make-withdraw`.

#lang sicp
