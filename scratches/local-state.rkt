; Exercise 3.0

#lang sicp

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin
        ; causes the expressions within to be evaluated in sequence
        ; and return value of the final expression
        (set! balance (- balance amount)))
      ("Insufficient funds")))

(define (new-withdraw)
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          ("Insufficient funds")))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      ("Insufficient funds"))))

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
