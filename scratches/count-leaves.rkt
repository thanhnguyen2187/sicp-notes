; Exercise ?.?

#lang sicp

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves (list (list 1 2)
                    (list 3 (list 4 5 6))
                    (list 4 6)
                    (list 6 8)))
