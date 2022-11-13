; Define a rule says that person 1 can replace person 2 if either person 1 does
; the same job as person 2, or someone who does person 1's job can also do
; person 2's job, and if person 1 and person 2 are not the same person. Using
; your rule, give queries that find the following:
;
; a. all people who can replace Cy D. Fect;
; b. all people who can replace someone who is being paid more than they are,
; together with the two salaries.

(load "ch4-query.scm")

(define microshaft-data-base-4-57
  (append microshaft-data-base
          '((rule (can-replace ?person-1 ?person-2)
                  (and (job ?person-1 ?position-1)
                       (job ?person-2 ?position-2)
                       (can-do-job ?position-1 ?position-2)
                       (not (same ?person-1 ?person-2)))))))

(initialize-data-base microshaft-data-base-4-57)

(query-driver-loop)

; rule
(can-replace ?x ?y)

; a.
(can-replace ?person (fect cy d))

; b.
(and (can-replace ?person-1 ?person-2)
     (salary ?person-1 ?amount-1)
     (salary ?person-2 ?amount-2)
     (lisp-value < ?amount-1 ?amount-2))
