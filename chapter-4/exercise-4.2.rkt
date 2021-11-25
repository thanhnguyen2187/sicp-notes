; Exercise 4.2
;
; Louis Reasoner plans to reorder the `cond` clauses in `eval`, so that the
; clause for procedure applications appears before the clause for assignments.
;
; He argues that this will make the interpreter more efficient: Since programs
; usually contain more applications than assignments, definitions, and so on,
; his modified `eval` will usually check fewer clauses than the original `eval`
; before identifying the type of an expression.
;
; a. What is wrong with Louis's plan? (Hint: what will Louis's evaluator do with
; the expression `(define x 3)`?)
;
; b. Louis is upset that his plan didn't work.
;
; He is willing to go to any lengths to make his evaluator recognize procedure
; applications before it checks for most other kinds of expression.
;
; Help him by changing the syntax of the evaluated language so that procedure
; applications start with `call`. For example, instead of `(factorial 3)`, we
; will now have to write (call factorial 3), and instead of `(+ 1 2)` we will
; have to write `(call + 1 2)`
;
; ---
; a. With Louis's change, we are going to have this evaluating order:
;
; - self-evaluating?
; - variable?
; - quoted?
; - application? (*)
; - assignment?
; - definition?
; - if?
; - lambda?
; - begin?
; - cond?
;
; `(define x 3)` is going to be evaluated following that order:
;
; - self-evaluating?: false
; - variable?: false
; - quoted?: false
; - application?: true
;
; Which leads us to `(apply (eval define env) (x 3))` that returns error.
;
; b. Our `application?` then would check if the first argument of `exp` is
; `call`.

#lang sicp

