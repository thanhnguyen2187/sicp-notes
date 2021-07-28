; Exercise 1.41
;
; Define a procedure `double` that takes a procedure of one argument as argument
; and returns a procedure that applies the orginal procedure twice.
;
; For example, if `inc` is a procedure that adds 1 to its argument, then (double
; inc) should be aprocedure that adds 2. What value is returned by
;
; ...

#lang sicp

(define (double f)
  (lambda (x)
    (f (f x))))

(define double-inc (double inc))
(((double (double double)) inc) 5)
