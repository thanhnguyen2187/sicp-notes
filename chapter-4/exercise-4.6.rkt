; Exercise 4.6
;
; `let` expressions are derived expressions, because
;
; ...
;
; is equivalent to
;
; ...
;
; Implement a syntactic transformation `let->combination` that reduces
; evaluating `let` expressions to evaluating combinations of the type shown
; above, and add the appropriate clause to `eval` to handle `let` expressions.

#lang sicp
