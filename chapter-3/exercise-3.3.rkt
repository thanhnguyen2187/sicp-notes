; Exercise 3.3
;
; Modify the `make-account` procedure so that it creates password-protected
; accounts. That is, `make-account` should take a symbol as an additional
; argument, as in
;
; ...
;
; The resulting account object should process a request only if it is
; accompanied by the password with which the account was created, and should
; otherwise return a complaint:
;
; ...

#lang sicp

(define (make-account balance
                      password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch input-password action)
    (if (eq? input-password password)
        (cond ((eq? action 'withdraw) withdraw)
              ((eq? action 'deposit)  deposit)
              (else (error "Unknown request: MAKE-ACOUNT " action)))
        (lambda (x) "Incorrect password")))

  dispatch)


(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
