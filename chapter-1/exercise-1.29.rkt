; Exercise 1.29
;
; Simpson's Rule is a more accurate method of numerical integration than the
; method illustrated above. Using Simpson's Rule, the integral of a function /f/
; between /a/ and /b/ is approximated as
;
; h/3 * (y0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n),
;
; where h = (b - a)/n, for some even integer n, and y_k = f(a + kh).
; (Increasing n increases the accuracy of the approximation.)
;
; Define a procedure that takes as arguments:
; - f
; - a
; - b
; - and n
; - and returns the value of the integral, computed using Simpson's Rule.
;
; Use your procedure to integrate `cube` between 0 and 1 (with n = 100, and n =
; 1000), and compare the results to those of the integral procedure shown above.

#lang sicp

(define (sum term
             lower
             next
             upper)
  (if (> lower upper)
      0
      (+ (term lower)
         (sum term (next lower) next upper))))

(define (cube x) (* x x x))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y-k k)
    (f (+ a
          (* k h))))
  (define (n-y-k n)
    (define (wrapper k)
      (* n 
         (y-k k)))
    wrapper)
  (define (term k)
    (cond [(or (= k 0)
               (= k n)) ((n-y-k 1.0) k)]
          [(even? k) ((n-y-k 2.0) k)]
          [else ((n-y-k 4.0) k)]))
  (define (next k) (+ k 1))
  (* (/ h 3.0)
     (sum term
          0
          next
          (+ n))))

(integral cube 0 1 100)
(integral cube 0 1 1000)
