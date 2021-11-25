; Exercise 3.41
;
; Ben Bitdiddle worries that it would be better to implement the bank acocunt as
; follows (where the commented line has been changed):
;
; ...
;
; because allowing unserialized access to the bank balance can result in
; anomalous behavior. Do you agree? Is there any scenario that demonstrates
; Ben's concern?
;
; ---
;
; There is no such scenario, since the accessing itself can be considered
; "atomic". Either the value is changed in time, or it is not, the accessed
; value is not damaged.

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

  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit)  (protected deposit))
            ((eq? m 'balance)  ((protected (lambda () balance))))
            (else (error "Unknown request: MAKE-ACCOUNT" m))))

    dispatch)
  )
