; Redesign the query language as a nondeterministic program to be implemented
; using the evaluator of Section 4.3, rather than as a stream prcess. In this
; approach, each query will produce a single answer (rather than the stream of
; all answers), and the user can type `try-again` to see more answers. You
; should find that much of the mechanism we built in this section is subsumed by
; nondeterministic search and backtracking. You will probably also find,
; however, that your new query language has subtle differences in behavior from
; the one implemented here. Can you find examples that illustrate the
; difference?
