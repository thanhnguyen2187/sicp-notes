; Cy D. Fect, looking forward to the day when he will rise in the organization,
; gives a query to find all the wheels (using the `wheel` rule of Section
; 4.4.1):
;
; ...
;
;
; To his surprise, the system responds:
;
; ...
;
; Why is Oliver Warbucks listed four times?
;
; ---
;
; Oliver Warbucks is listed four times, since he is the "grand-supervisor"
; (a supervisor's supervisor) of four people: Robert Cratchet, Lem E. Tweakit,
; Cy D. Fect, and Alyssa P. Hacker, while Ben Bitdiddle is the
; "grand-supervisor" of Louis Reasoner.

(load "ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

(wheel ?who)

(and (supervisor ?middle-manager ?person)
     (supervisor ?x ?middle-manager))

(supervisor ?subordinate ?supervisor)
