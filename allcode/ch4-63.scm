; The following data base (see Genesis 4) traces the genealogy of the
; descendants of Ada back to Adam, by way of Cain:
;
; ...
;
; Formulate rules such as "if S is the son of f, and f is the son of G, then S
; is the grandson of G", and "if W is the wife of M, and S is the son of W, then
; S is the son of M" (which was supposedly more true in biblical times than
; today) that will enable the query system to find the grandson of Cain; the
; sons of Lamech; the grandsons of Methushael. (See Exercise 4.69 for some rules
; to deduce more complicated relationships)

(load "ch4-query.scm")

(define data-base-4-61
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
(rule (grandson ?person ?grandson)
      (and (son ?person ?son)
           (son ?son ?grandson)))
(rule (son ?person ?son)
      (and (wife ?person ?wife)
           (son ?wife ?son)))
))

(initialize-data-base data-base-4-61)

(query-driver-loop)

(grandson cain ?x)

(grandson methushael ?x)

