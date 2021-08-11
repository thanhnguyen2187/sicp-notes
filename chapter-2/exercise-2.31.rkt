; Exercise 2.31
;
; Abstract your answer to Exercise 2.30 to produce a procedure `tree-map` with
; the property that `square-tree` could be defined as
;
; ...

#lang sicp

(define (square x) (* x x))
(define (tree-map f tr)
  (cond ((null? tr) nil)
        ((pair? tr) (map (lambda (tr) (tree-map f tr))
                         tr))
        (else (f tr))))

(define (square-tree tr)
  (tree-map square
            tr))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

