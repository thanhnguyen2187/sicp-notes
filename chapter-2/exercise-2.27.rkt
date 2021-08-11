; Exercise 2.27
;
; Modify your `reverse` procedure of Exercise 2.18 to produce a `deep-reverse`
; procedure that takes a list as argument and returns its value the list with
; its elements reversed and with all sublists deep-reversed as well.
;
; For example,
;
; ...

#lang sicp

(define (deep-reverse xs)
  (if (null? xs)
      (list)
      (append (deep-reverse (cdr xs))
              (list (if (pair? (car xs))
                        (deep-reverse (car xs))
                        (car xs))))))

(define L (list (list 1 2 (list 9 8 7)) 2 3 4 5))
(list? (list 1 2 3))
(list? 1)
(list? nil)

(deep-reverse L)
