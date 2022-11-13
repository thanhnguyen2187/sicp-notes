; Louis Reasoner mistakenly deletes the `outranked-by` rule (Section 4.4.1) from
; the data base. When he realizes this, he quickly reinstalls it. Unfortunately,
; he makes a slight change in the rule, and types it in as
;
; ...
;
; Just after Louis types this information into the system, De-Witt Aull comes by
; to findout who outranks Ben Bitdiddle. He issues the query:
;
; ...
;
; After answering, the system goes into an infinite loop. Explain why?
;
; ---
;
; The system goes into an infinite loop since `outranked-by` in the modified
; version call itself without using `supervisor` as a filter.

; modified version
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

; original version
(rule (outranked-by ?staff-person ?boss)
      (or (supervsor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
