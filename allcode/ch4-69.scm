; Beginning with the data base and the rules you formulated in Exercise 4.63,
; devise a rule for adding "greats" to a grandson relationship. This should
; enable the system to deduce that Irad is the great-grandson of Adam, or Jabal
; and Jubal are the great-great-great-great-great-grandsons of Adam. (Hint:
; represent the fact about Irad, for example, as `((great grandson) Adam Irad)`.
; Write rules that determine if a list ends in the word `grandson`. Use this to
; express a rule that allows one to derive the relationship `((great . ?rel) ?x
; ?y)`, where `?rel` is a list ending in `grandson`.) Check your rules on
; queries such as `((great grandson) ?g ?ggs)` and `(?relationship Adam Irad)`

(load "ch4-query.scm")

(define data-base-4-69
  '(
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)
;
; (rule (last (?item) ?item))
; (rule (last (?first-item . ?rest-items) ?last-item)
;       (and (not (same-2 (?rest-items ())))
;            (last ?rest-items ?last-item)))
;
(lower grandson son)
(lower (great grandson) grandson)
; (lower (great great grandson) (great grandson))
(rule (lower (great . ?relationships) ?relationships)
      (same-2 ?relationships (great . ?other-relationships))
      ; (and (not (same-2 ?relationships ()))
      ;      (same-2 ?relationships (great . ?other-relationships)))
      )
;
(rule (same-2 ?x ?x))
; (rule (same-3 ?x ?x ?x))
(rule (son ?person ?son)
      (and (wife ?person ?wife)
           (son ?wife ?son)))
(rule (grandson ?person ?grandson)
      (and (son ?person ?son)
           (son ?son ?grandson)))
(rule ((great . ?relationships) ?person-1 ?person-3)
      (and (son ?person-1 ?person-2)
           (lower (great . ?relationships) ?another-relationship)
           (?another-relationship ?person-2 ?person-3)
           ))
))

(initialize-data-base data-base-4-69)

(query-driver-loop)

(restart 1)

(?relationship adam ?person)

(?relationship cain ?person)

(and (same-2 (great . ?relationships) (great great grandson))
     (?relationship ?person-1 ?person-2))

(lower (great great great grandson) ?relationship)

(lower (great great grandson) ?relationship)

(lower (great grandson) ?relationship)

(lower grandson ?relationship)

(lower son ?relationship)

