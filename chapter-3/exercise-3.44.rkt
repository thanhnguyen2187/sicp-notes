; Exercise 3.44
;
; Consider the problem of transferring an amount from one account to another.
; Ben Bitdiddle claims that this can be accomplished with the following
; procedure, even if there are multiple people concurrently transferring money
; amoung multiple accounts, using any account mechanism that serializes deposit
; and withdrawl transactions, for example, the version of `make-account` in the
; text above.
;
; ...
;
; Louis Reasoner claims that there is a problem here, and that we need to use a
; more sophisticated method, such as the one required for dealing with the
; exchange problem. Is Louis right? If not, what is the essential difference
; between the transfer problem and the exchange problem? (You should assume that
; the blance in `from-account` is at least `amount`.)
;
; ---
;
; Louis is wrong. The procedure is not atomic, but it will eventually correct.
; Let us have a look at the two actions:
;
; 1. Withdraw from the first account
; 2. Deposit to the second account
;
; As long as `withdraw` and `deposit` is atomic, even if we have a lot of
; `transfer`s going on, the `balance`s of the accounts will eventually be
; correct. They are **order independent**.
;
; For the exchange problem, we essentially have five actions, and they are
; **order dependent**.

#lang sicp

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))
