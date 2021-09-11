; Exercise 3.7
;
; Consider the bank account objects created by `make-account`, with the password
; modification described in Exercise 3.3. Suppose that our banking system
; requires the ability to make joint accounts. Define a procedure `make-joint`
; that accomplishes this.
;
; `make-joint` should take three arguments.
; - The first is a password-protected account.
; - The second argument must match the password with which the amount was
; defined in order for the `make-joint` operation to proceed.
; - The third argument is a new password. `make-joint` is to create an
; additional access to the original account using the new password. For example,
; if `peter-acc` is a bank account with password `open-sesame`, then 
;
; ...
;
; will allow one to make transactions on `peter-acc` using the name `paul-acc`
; and the password `rosebud`. You may wish to modify your solution to Exercise
; 3.3 to accomodate this new feature.

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

(define (make-joint account
                    first-password
                    second-password)
  (define (withdraw amount)
    ((account first-password 'withdraw) amount))
  (define (deposit amount)
    ((account first-password 'deposit) amount))

  (define (dispatch input-password action)
    (if (eq? input-password second-password)
        (cond ((eq? action 'withdraw) withdraw)
              ((eq? action 'deposit)  deposit)
              (else (error "Unknown request: MAKE-JOINT " action)))
        (lambda (x) "Incorrect password")))

  dispatch)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'deposit) 10)
((peter-acc 'open-sesame 'deposit) 10)
