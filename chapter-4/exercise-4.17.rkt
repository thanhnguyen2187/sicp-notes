; Exercise 4.17
;
; Draw diagrams of the environment in effect when evaluating the expression
; *<e3>* in the procedure in the text, comparing how this will be structured
; when definitions are interpreted sequentially with how it will be structured
; if definitions are scanned out as described.
;
; - Why is there an extra frame in the transformed program?
; - Explain why this difference in environment structure can never make a
; difference in the behavior of a correct program.
; - Design a way to make the interpreter implement the "simultaneous" scope rule
; for internal definitions without constructing the extra frame.
;
; ---
;
; - Draw diagrams of the environment in effect when evaluating the expression
;   *<e3>* in the procedure in the text
;
; ```
; - lambda <vars>
;   - let
;     u
;     v
;     <e3>
;
; - lambda <vars>
;   u
;   v
; ```
;
; - Comparing how this will be structured when definitions are interpreted
;   sequentially with how it will be structured if definitions are scanned out
;   as described.
;
; There is going to be another frame for when we do transform the definitions.
;
; - Why is there an extra frame in the transformed program?
;
; Because there is a `let` outside.
;
; - Explain why this difference in environment structure can never make a
; difference in the behavior of a correct program.
;
; It does not make a difference since we change nothing to the execution of the
; program.

#lang sicp

(define <e1> 0)
(define <e2> 0)
(define <e3> 0)

(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)

; - Design a way to make the interpreter implement the "simultaneous" scope rule
; for internal definitions without constructing the extra frame.

(lambda <vars>
  (define u '*unassigned*)
  (define v '*unassigned*)
  (set! u <e1>)
  (set! v <e2>)
  <e3>)

