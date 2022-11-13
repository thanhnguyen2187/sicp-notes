; The following rules implement a `next-to` relation that finds adjacent
; elements of a list:
;
; ...
;
; What will the response be to the following queries?
;
; ...

(load "ch4-query.scm")

(define data-base-4-60
  '(
(rule (?x next-to ?y in (?x ?y . ?u)))
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))
))

(initialize-data-base data-base-4-60)

(query-driver-loop)

(?x next-to ?y in (1 (2 3) 4))
; ((2 3) next-to 4 in (1 (2 3) 4))
; (1 next-to (2 3) in (1 (2 3) 4))

(?x next-to 1 in (2 1 3 1))
; (3 next-to 1 in (2 1 3 1))
; (2 next-to 1 in (2 1 3 1))

(?x next-to ?y in (1 2))

