; Exercise 3.18
;
; Write a procedure that examines a list and determines whether it contains a
; cycle, that is, whether a program that tried to find the end of the list by
; taking successive `cdrs` would go into an infinite loop. Exercise 3.13
; constructed such lists.

#lang sicp

(define (contain-cycle? x)
  (define (within encountered-pairs x)
    (if (null? encountered-pairs)
        false
        (or (eq? (car encountered-pairs) x)
            (within (cdr encountered-pairs) x))))
  (define (recurse encountered-pairs x)
    (cond ((not (pair? x)) false)
          ((within encountered-pairs x) true)
          (else (let ((newly-encountered-pairs (cons x encountered-pairs)))
                  (or (recurse newly-encountered-pairs (car x))
                      (recurse newly-encountered-pairs (cdr x)))))))
  (recurse (list) x))

(define x (list '1 '2))
(set-cdr! x x)

(contain-cycle? x)
(contain-cycle? (list 1 2 3))
