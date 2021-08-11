; Exercise 2.55
;
; Eva Lu Ator types to interpreter the expressions
;
; ...
;
; To her suprise, the interpreter prints back `quote`. Explain.

#lang sicp

(car ''abracadabra)
; The expression becomes `(car (quote (quote abracadabra)))`,
; therefore the interpreter prints back `quote`.
