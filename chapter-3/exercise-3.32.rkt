; Exercise 3.32
;
; The procedures to be run during each time segment of the agenda are kept in a
; queue. Thus, the procedures for each segment are called in the order in which
; they were added to the agenda (first in, first out).
;
; Explain why this order must be used.
;
; In particular, trace the behavior of an and-gate whose input change from 0, 1
; to 1, 0 in the same segment and say how the behavior would differ if we stored
; a segment's procedures in an ordinary list, adding and removing procedures
; only at the front (last in, first out).
;
; ---
;
; We can look at the agenda like this:
;
; ```
; Agenda:
; - 0
; - 1: [#procedure input 0 1] [#procedure input 1 0]
; - 3: [#procedure] [#procedure] [#procedure]
; ```
; 
; In the case, using a stack (last in, first out) versus a queue (first in,
; first out) will not make a different. In other cases, it may result of strange
; behaviors.

#lang sicp
