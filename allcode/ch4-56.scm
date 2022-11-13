; Formulate compound queries that retrieve the following information:
;
; a. the names of all people who are supervised by Ben Bitdiddle, together with
; their addresses;
; b. all people whose salary is less than Ben Bitdiddle's, together with their
; salary and Ben Bitdiddle's salary;
; c. all people who are supervised by someone who is not in the computer
; division, together with the supervisor's name and job.

(load "ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

; a.
(and (supervisor ?person (bitdiddle ben))
     (address ?person ?where))

; b.
(and (salary (bitdiddle ben) ?ben-amount)
     (salary ?person ?amount)
     (lisp-value < ?amount ?ben-amount))

; c.
(and (supervisor ?person ?supervisor)
     (not (job ?supervisor (computer . ?x)))
     (job ?supervisor ?division))

