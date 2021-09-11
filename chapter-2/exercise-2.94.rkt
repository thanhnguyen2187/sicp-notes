; Exercise 2.94
;
; Using `div-terms`, implement the procedure `remainder-terms` and use this to
; define `gcd-terms` above. Now write a procedure `gcd-poly` that computes the
; polynomial `GCD` of two polys. (The procedure should signal an error if the
; two polys are not in the same variable.)
;
; Install in the system a generic operation `greatest-common-divisor` that
; reduces to `gcd-poly` for polynomials and to ordinary `gcd` for ordinary
; numbers.
;
; As a test, try
;
; ...
;
; and check your result by hand.

#lang sicp

(define (make-polynomial) 0)
(define (greatest-common-divisor) 0)

(define (remainder-terms) 0)
(define (empty-termlist?) 0)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 -1) (1 -1))))
(greatest-common-divisor p1 p2)
