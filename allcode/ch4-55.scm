; Give simple queries that retrieve the following information from the data
; base:
;
; 1. all people supervised by Ben Bitdiddle
; 2. the names and jobs of all people in the accounting division
; 3. the names and addresses of all people who live in Slumerville

(load "ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

; 1.
(supervisor ?x (bitdiddle ben))

; 2.
(job ?x (accounting . ?y))

; 3.
(address ?x (Slumerville . ?y))
