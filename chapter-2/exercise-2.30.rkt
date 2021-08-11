; Exercise 2.30
;
; Define a procedure `square-tree` analogous to the `square-list` procedure of
; Exercise 2.21. That is, `square-tree` should behave as follows:
;
; ...

#lang sicp

(define (square x) (* x x))

(define (square-tree tr)
  (cond ((null? tr) nil)
        ((pair? tr) (cons (square-tree (car tr))
                          (square-tree (cdr tr))))
        (else (square tr))))
(define (square-tree-v2 tr)
  (cond ((null? tr) nil)
        ((pair? tr) (map square-tree-v2 tr))
        (else (square tr))))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(square-tree-v2
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
