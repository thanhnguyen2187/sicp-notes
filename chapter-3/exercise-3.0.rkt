; Exercise 3.0

#lang sicp

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit)  deposit)
          (else (error "Unknown request: MAKE-ACOUNT " m))))
  dispatch)

; Exercise 3.1
;
; An *accumulator* is a procedure that is called repeatedly with a single
; numeric argument and accumulates its arguments into a sum. Each time it is
; called, it returns the currently accumulated sum.
;
; Write a procedure `make-accumulator` that generate accumulators, each
; maintaining an independent sum. The input to `make-accumulator` should specify
; the initial value of the sum; for example
;
; ...

(define (make-accumulator))
