; Exercise 2.6
;
; In case representing pairs as procedures wasn't mind-boggling enough, consider
; that, in a language that can manipulate procedures, we can get by without
; numbers (at least insofar as nonnegative integers are concerned) by
; implementing 0 and the operating of adding 1 as
; 
; ...
;
; This representation is known as the /Church numerals/, after its inventor,
; Alonzo Church, the logician who invented the lambda-calculus.
;
; Define `one` and `two` directly (not in terms of `zero` and `add-1`).
; (Hint: Use substitution to evaluate `(add-1 zero)`). Give a direct definition
; of the addition procedure `+` (not in terms of repeated application `add-1`).

#lang sicp

(define zero (lambda (f)
               (lambda (x) x)))
(define one (lambda (f)
              (lambda (x) (f x))))
(define two (lambda (f)
              (lambda (x) (f (f x)))))
(define (add-1 n) 
  (lambda (f) (lambda (x)
                (f ((n f) x)))))
; (add-1 zero)
; (lambda ((lambda (f) (lambda (x) x))))

(define (add x1 x2)
  (lambda (f)
    (lambda (x)
      ((x2 f) ((x1 f) x)))))

(((add one two) inc) 0)

; (add-1 one)
; (add-1 two)
