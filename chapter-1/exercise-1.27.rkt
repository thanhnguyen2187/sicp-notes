; Exercise 1.27
;
; Demonstrate that the Carmichael numbers listed in Footnote 1.47 really do fool
; the Fermat test. That is, write a procedure that takes an integer n and tests
; wheter a^n is congruent to a modulo n for every a < n, and try your procedure
; on the given Carmichael numbers.

#lang sicp

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (test n)
  (define (iter a b)
    (cond ((< a b)
           (= (expmod n a a)))
          )))
