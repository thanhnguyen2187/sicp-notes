; Exercise 1.18
; 
; Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that
; generates an iterative process for multiplying two integers in terms of
; adding, doubling, and halving and uses a logarithmic number of steps.

#lang sicp

(define (is-even? x)
  (= (remainder x 2) 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multiply a b)
  (define (iter a b c)
    (cond ((= b 0) c)
          ((= b 1) (+ a c))
          ((is-even? b) (iter (double a)
                              (halve b)
                              c))
          (else (iter (double a)
                      (halve (- b 1))
                      (+ c a)))))
  (iter a b 0))

(multiply 6 7)
(multiply 7 8)
