; Exercise 2.18
;
; Define a procedure `reverse` that takes a list as argument and returns a list
; of the same elemnts in reverse order:

#lang sicp

(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l))
              (list (car l)))))

#| (append (list 1 2 3) |#
#|         (list 3 2 1)) |#

(reverse (list 1 2 3 4 5))
