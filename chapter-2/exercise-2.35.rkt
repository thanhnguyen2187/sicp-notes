; Exercise 2.35
;
; Redefine `count-leaves` from Seciton 2.2.2 as an accumulation:
;
; ...

#lang sicp

(define ? 0)
(define (accumulate operator
                    initial
                    sequence)
  (if (null? sequence)
      initial
      (operator (car sequence)
                (accumulate operator
                            initial
                            (cdr sequence)))))

(define (count-leaves t)
  #| (display t) |#
  #| (newline) |#
  #| (display (pair? t)) |#
  #| (newline) |#
  (accumulate (lambda (t accumulated)
                (+ accumulated
                   (if (pair? t)
                       (count-leaves t)
                       1)))
              0
              (map identity t)))
(count-leaves (list (list 1 2)
                    (list 3 (list 4 5 6))
                    (list 4 6)
                    (list 6 8)))
