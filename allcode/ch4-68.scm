; Define rules to implement the `reverse` operation of Exercise 2.18, which
; returns a list containing the same elements as a given list in reverse order.
; (Hint: Use `append-to-form`.) Can your rule answer both `(reverse (1 2 3) ?x)`
; and `(reverse ?x (1 2 3))`
;
; ---
;
; The rule cannot answer both `(reverse (1 2 3) ?x)` and `(reverse ?x (1 2 3))`,
; since its implementation only allows "one-way" list destructuring.

(load "ch4-query.scm")

(define data-base-4-68
  '(
;
(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))
;
(rule (reverse () ()))
; (rule (reverse (?x) (?x)))
(rule (reverse (?x . ?y) ?z)
      (and (reverse ?y ?u)
           (append-to-form ?u (?x) ?z)))
))

(initialize-data-base data-base-4-68)

(restart 1)

(query-driver-loop)

(reverse (1) ?x)

(reverse (1 2 3) ?x)

(reverse ?x (1 2 3))

