#lang sicp

(define (square x)
  (* x x))

(define (sum-square a b)
  (+ (square a) (square b)))

(define (>= a b)
  (or (> a b) (= a b)))

(define (f a b c)
  (cond ((= (min a b c) c) (sum-square a b))
        ((= (min a b c) a) (sum-square b c))
        ((= (min a b c) b) (sum-square c a))
        (else 0)))

(f 3 4 5)
(f 2 2 5)
(f 1 2 5)
(f 1 1 5)
