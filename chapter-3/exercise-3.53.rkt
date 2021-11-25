; Exercise 3.53
;
; Without running the program, describe the elements of the stream defined by
;
; ...
;
; The elements are powers of 2 (1, 2, 4, 8...).

#lang sicp

(define s (cons-stream 1 (add-stream s s)))

