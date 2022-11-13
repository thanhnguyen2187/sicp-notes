; By giving the query
;
; ...
;
; Alyssa P. Hacker is able to find people who live ner her, with whom she can
; ride to work. On the other hand, when she tries to find all pairs of people
; who live near each other by querying
;
; ...
;
; she notices that each pair of people who live near each other is listed twice;
; for example,
;
; Why does this happen? Is there a way to find a list of people who live near
; each other, in which each pair appears only once? Explain.

(load "ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

(lives-near ?person-1 ?person-2)

; Each pair of people who live near each other is listed twice since the rule
; `lives-near` is a bidirectional rule (if `person-1` lives near `person-2`,
; the other way around is also true). 

(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))

(and (not (job ?x (computer programmer)))
     (supervisor ?x ?y))
