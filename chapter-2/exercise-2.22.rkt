; Exercise 2.22
; 
; Louis Reasoner tries to rewrite the first `square-list` procedure of Exercise
; 2.21 so that it involves an iterative process:
;
; ...
;
; Unfortunately, defining `square-list` this way produces the answer list in the
; reverse order of the one desired. Why?
;
; Louis then tries to fix his bug by intercharging the arguments to `cons`:
;
; ...
;
; This doesn't work either. Explain.
;
; ---
;
; In the first case (cons (square (car things)) answer) give us the reversed
; answer since the front of the number in the original list get put into the
; front of the result list, causing the reversal.
;
; Original List: 1 2 3 4 5
; Result List:
;
; Original List: 2 3 4 5
; Result List: 1
; 
; Original List: 3 4 5
; Result List: 2 1
;
; ...
;
; In the second case, the format of the result list is not a valid list, hence
; the "weird" display.

#lang sicp

(define (square x) (* x x))

(define (square-list items)
  (define (iter things
                answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(define L (list 1 2 3 4 5))

(cons 2 (cons 1 nil))

(square-list L)
