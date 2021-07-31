; Exercise 2.5
;
; Show that we can represent pairs of nonnegative integers using only numbers
; and arithmetic operations if we represent the pair /a/ and /b/ as the integer
; that is the product /2^a * 3^b/.
;
; Give the corresponding definitions of the procedures `cons`, `cars`, and
; `cdr`.

#lang sicp

(define (square x) (* x x))
(define (nth-power b n)
  (cond ((= n 0) 1)
        ((= n 1) b)
        ((even? n) (square (nth-power b
                                      (/ n 2))))
        (else (* b
                 (square (nth-power b
                                    (/ (- n 1)
                                       2)))))))
(define (divisible-by a b)
  (= (remainder a b) 0))
(define (divisible-by-2 a)
  (divisible-by a 2))
(define (divisible-by-3 a)
  (divisible-by a 3))

(define (greatest-power b a)
  (define (iterate n current next)
    (if (divisible-by a
                      next)
        (iterate (+ n 1)
                 next
                 (* next b))
        n))
  (iterate 0 1 b))

(define (make-cons b1 b2)
  (lambda (a1 a2) (* (nth-power b1 a1)
                     (nth-power b2 a2))))
(define (make-car b1)
  (lambda (a) (greatest-power b1 a)))
(define (make-cdr b2)
  (lambda (a) (greatest-power b2 a)))

(define b1 2)
(define b2 3)
(define cons (make-cons b1 b2))
(define car (make-car b1))
(define cdr (make-cdr b2))

(define a (cons 5 4))
(car a)
(cdr a)

