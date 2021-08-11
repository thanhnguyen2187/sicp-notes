; Exercise 2.28
;
; Write a procedure `fringe` that takes as argument a tree (represented as a
; list) and returns a list whose elements are all the leaves of the tree
; arranged in left to right order. For example,
;
; ...

#lang sicp

(define (flatten xs)
  (cond ((null? xs) (list))
        ((not (pair? xs)) (list xs))
        (else (append (flatten (car xs))
                      (flatten (cdr xs))))))

(define (length xs)
  (display xs)
  (newline)
  (if (null? xs)
      0
      (+ 1
         (length (cdr xs)))))

(define (fringe tr)
  (if (null? tr)
      (list)
      (append (if (pair? tr)
                  (flatten tr)
                  #t))))

; (null? (list))
; (flatten (list 1 2 (list 3 4) (list 5 6)))
(define L (list 1
                2
                (list 3 4)
                (list 3 (list 8 8 8))
                (list 5 6)))
(define L2 1)
(flatten L)
