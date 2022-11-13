; Define a rule that says that a person is a "big shot" in a division if the
; person works in the division but does not have a supervisor who works in the
; division.

(load "ch4-query.scm")

(define microshaft-data-base-4-58
  (append microshaft-data-base
          '((rule (is-big-shot ?person)
                  (and (job ?person (?division . ?position))
                       (or (and (supervisor ?person ?supervisor)
                                (job ?supervisor (?supervisor-division . ?supervisor-position))
                                (not (same ?division ?supervisor-division)))
                           (not (supervisor ?person ?not-supervisor))))))))

(initialize-data-base microshaft-data-base-4-58)

(query-driver-loop)

(is-big-shot ?person)

