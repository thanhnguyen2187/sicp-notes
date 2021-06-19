; Exercise 1.20 The process that procedure generates is of course dependent on
; the rules used by the intepreter. As an example, consider the iterative `gcd`
; procedure given above. Suppose we were to interpret this procedure using
; normal-order-evaluation, as discussed in Section 1.1.5 (The
; normal-order-evaluation rule for `if` is described in Exercise 1.5).
;
; Using the substitution method (for normal order), illustrate the process
; generated in evaluating (`gcd 206 40`) and indicate the remainder operations
; that are actually performed.
;
; How many `remainder` operations that are actually performed in the normal-order
; evaluation of (gcd 206 40)? In the applicative-order evaluation?

#lang sicp

; gcd 206 40
; gcd 40 6
; gcd 6 4
; gcd 4 2
; gcd 2 0
; 0

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

; normal-order
(gcd 40
     (remainder 206 40))
(gcd (remainder 206 40)
     (remainder 40
                (remainder 206 40)))
(gcd 6
     (remainder 40
                6)) ; 2
(gcd 6 4) ; 3
(gcd 4
     (remainder 6 4))
(gcd (remainder 6 4)
     (remainder 4
                (remainder 6 4)))
(gcd 2
     (remainder 4 2)) ; 5
(gcd 2 0) ; 6

; applicative-order
(gcd 40
     (remainder 206 40))
(gcd 40 6) ; 1
(gcd 6
     (remainder 40 6))
(gcd 6 4) ; 2
(gcd 4 (remainder 6 4))
(gcd 4 2) ; 3
(gcd 2 (remainder 4 2))
(gcd 2 0) ; 4
