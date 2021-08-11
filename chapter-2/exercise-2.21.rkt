; Exercise 2.21
;
; The procedure `square-list` takes a list of numbers as argument and returns a
; list of the squares of those numbers.
;
; Here are two different definitions of `square-list`. Complete both of them by
; filling in the missing expressions:
;
; ...

#lang sicp

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-v2 items)
  (map square
       items))

(define L (list 1 2 3 4 5))
(square-list L)
(square-list-v2 L)
