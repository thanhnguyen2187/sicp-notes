; Exercise 3.13
;
; Consider the following `make-cycle` procedure, which uses the `last-pair`
; procedure defined in Exercise 3.12:
;
; ...
;
; Draw a box-and-pointer diagram that shows the structure `z` created by:
;
; ...
;
; What happens if we try to compute `(last-pair z)`?
;
; ---
;
; If we try to compute `(last-pair z)`, it becomes an infinite loop.

#lang sicp

(define (last-pair x) 0)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
; [a] -> [b] -> [c]
;  ^             |
;  |             |
;  +-------------+
