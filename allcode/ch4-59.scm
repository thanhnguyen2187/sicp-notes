; Ben Bitdiddle has missed one meeting too many. Fearing that his habit of
; forgetting mettings could cost him his job, Ben decides to do something about
; it. He adds all the weekly meetings of the firm to the Microshaft data base by
; asserting the following:
;
; ...
;
; Each of the above assertion is for a meeting of an entire division. Ben also
; adds an entry for the company-wide meeting that spans all the divisions. All
; of the company's employees attend this meeting.
;
; ...
;
; a. On Friday morning, Ben wants to query the data base for all the meetings
; that occur that day. What query should he use?
;
; b. Alyssa P. Hacker is unimpressed. She thinks it would be much more useful to
; be able to ask for her meetings by specifying her name. So she designs a rule
; that says that a person's meetings include all `whole-company` meetings plus
; all meetings that of that person's division. Fill in the body of Alyssa's
; rule.
;
; ...
;
; c. Alyssa arrives at work on Wednesday morning and wonders what meetings she
; has to attend that day. Having defined the above rule, what query should she
; make to find this out?

(load "ch4-query.scm")

(define microshaft-data-base-4-59
  (append microshaft-data-base
          '(
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
(meeting whole-company (Wednesday 4pm))

; b.
(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?division . ?x))
           (or (meeting ?division ?day-and-time)
               (meeting whole-company ?day-and-time))))
)))

(initialize-data-base microshaft-data-base-4-59)

(query-driver-loop)

(restart 1)

; a.
(meeting ?division (friday ?time))

; b.
(meeting-time ?person ?day-and-time)

; c.
(meeting-time (hacker alyssa p) ?day-and-time)
