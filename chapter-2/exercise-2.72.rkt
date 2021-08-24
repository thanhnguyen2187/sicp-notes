; Exercise 2.72
;
; Consider the encoding procedure that you designed in Exercise 2.68. What is
; the order of growth in the number of steps needed to search the symbol list at
; each node encounterd. To answer this question in general is difficult.
; Consider the special case where the relative frequencies of the /n/ symbols
; are as described in Exercise 2.71, and give the order of growth (as a function
; of /n/) of the number of steps needed to encode the most frequent and least
; frequent symbols in the alphabet.

#lang sicp

; f_min(n) = 1
; f_max(n) = (n - 1)
