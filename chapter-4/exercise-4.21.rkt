; Exercise 4.21
;
; Amazingly, Louis's intuition in Exercise 4.20 is correct. It is indeed
; possible to specify recursive procedures without using `letrec` (or even
; `define`), although the method for accomplishing this is much more subtle than
; Louis imagined.
;
; The following expression computes 10 factorial by applying a recursive
; factorial procedure:
;
; ...
;
; a. Check (by evaluating the expression) that this really does compute
; factorials. Devise an analogous expression for computing Fibonacci number.
;
; b. Consider the following procedure, which includes mutually recursive
; internal definitions:
;
; ...
;
; Fill in the missing expressions to complete an alternative definition of `f`,
; which uses neither internal definitions nor `letrec`:
;
; ...

#lang sicp

((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k)
      (if (= k 1)
        1
        (* k (ft ft (- k 1)))))))
 8)

((lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (ft k)
      (cond
        ((= k 0) 0)
        ((= k 1) 1)
        (else (+ (ft ft (- k 1))
                 (ft ft (- k 2))))))))
 9)

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
       true
       (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
       false
       (ev? ev? od? (- n 1))))))

(f 8)
