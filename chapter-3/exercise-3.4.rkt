; Exercise 3.4
;
; Modify the `make-account` procedure of Exercise 3.3 by adding another local
; state variable so that, if an account is accessed more than seven consecutive
; times with an incorrect password, it invokes the procedure `call-the-cops`.

#lang sicp

(define (make-account balance
                      password)

  (define incorrect-password-count 0)

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
        (begin (set! incorrect-password-count (inc incorrect-password-count))
               (if (>= incorrect-password-count 7)
                   (lambda (x) "Called the cops!")
                   (lambda (x) "Incorrect password"))
               )))

  dispatch)


(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
