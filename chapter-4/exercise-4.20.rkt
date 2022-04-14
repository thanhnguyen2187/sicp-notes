; Exercise 4.20
;
; Because internal definitions look sequential but are actually simultaneous,
; some people prefer to avoid them entirely, and use the special form `letrec`
; instead. `letrec` looks like let, so it is not surprising that the variables
; it binds are bound simultaneously and have the same scope as each other. The
; sample procedure `f` above can be written without internal definitions, but
; with exactly the same meaning, as
;
; ...
;
; `letrec` expressions, which have the form
;
; ...
;
; are a variation on `let` in which the expressions *<exp_k>* that provide the
; initial values for the variables *<var_k>* are evaluated in an environment
; that includes all the `letrec` bindings. This permits recursion in the
; bindings, such as the mutual recursion of `even?` and `odd?` in the example
; above, or the evaluation of 10 factorial with
;
; ...
;
; a. Implement `letrec` as a derived expression, by transforming a `letrec`
; expression into a `let` expression as shown in the text above or in Exercise
; 4.18. That is, the `letrec` variables should be created with a `let` and then
; be assigned to their values with `set!`.
;
; b. Louis Reasoner is confused by all this fuss about internal definitions. The
; way he sees it, if you don't like to use `define` inside a procedure, you can
; just use `let`.
;
; Illustrate what is loose about his reasoning by drawing an environment diagram
; that shows the environment in which the *<rest of body of f>* is evaluated
; during evaluation of the expression `(f 5)`, with `f` defined as in this
; exercise.
;
; Draw an environment diagram for the same evaluation, but with `let` in place
; of `letrec` in the definition of `f`.
;
; ---
;
; b.
;
; For "normal" `letrec`:
;
;   Global environment
;     f
;       even?
;       odd?
;       __even?
;       __odd?
;
; Within `f`, `__even?` asks for `odd?`, and `__odd?` asks for `even?`, which
; exists beforehands. Recursion in the bindings thus is enabled.
;
; For `let`-replaced `letrec`:
;
;   Global environment
;     f
;       even?
;       odd?
;
; Within `f`, `even?` asks for `odd?` which is not defined nor exist at the
; time. Recursion in the binding cannot be done here.

#lang sicp

(define <rest-of-body-of-f> 0)

(define (f x)
  (letrec
    ((even? (lambda (n)
              (if (= n 0)
                true
                (odd? (- n 1)))))
     (odd? (lambda (n)
             (if (= n 0)
               false
               (even? (- n 1))))))
    <rest-of-body-of-f>))
