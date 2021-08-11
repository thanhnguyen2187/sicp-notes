; Exercise 2.18
;
; Define a procedure `reverse` that takes a list as argument and returns a list
; of the same elemnts in reverse order:

#lang sicp

(define (last l)
  (if (null? (cdr l))
      (car l)
      (last (cdr l))))

(define (reverse l)
  (if (or (null? l)
          (null? (cdr l)))
      l
      (cons (reverse (cdr l))
            (car l))))

#| (append (list 1 2 3) |#
#|         (list 3 2 1)) |#

(reverse (list 5))
(reverse (list 1 2))
(reverse (list 1 2 3 4 5))
; (last (list 1 2 3))
