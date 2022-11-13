; Define rules to implement the `last-pair` operation of Excercise 2.17, which
; returns a list containing the last elements of a nonempty list. Check your
; rules on queries such as `(last-pair (3) ?x)`, `(last-pair (1 2 3) ?x)`, and
; `(last-pair (2 ?x) (3))`. Do your rules work correctly on queries such as
; `(last-pair ?x (3))`

(load "ch4-query.scm")

(define data-base-4-61
  '(
(rule (last-pair of (?x . ?y) is (?z))
      (last-pair of ?y is (?z)))
(rule (last-pair of (?y) is (?y)))
))

(initialize-data-base data-base-4-61)

(query-driver-loop)

(last-pair of (3) is ?x)

(last-pair of (1 2 3) is ?x)

(last-pair of (2 ?x) is (3))

(last-pair of ?x is (3))

