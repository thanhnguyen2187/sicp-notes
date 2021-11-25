; Exercise 3.48
;
; Explain in detail why the deadlock-avoidance method described above, (i.e.,
; the accounts are numbered, and each process attempts to acquire the
; smaller-numbered account first) avoids deadlock in the exchange problem.
;
; Rewrite `serialized-exchange` to incorporate this idea. (You will also need to
; modify `make-account` so that each accound is created with a number, which can
; be accessed by sending an appropriate message.)
;
; ---
;
; `exchange` works like this:
;
; - Withdraw from `account1`
; - Deposit to `account2`
;
; In which we locks both account until the exchanging is done. The actions are:
;
; - Lock `account1`
; - Lock `account2`
; - Do the `exchange`-ing
;
; Suppose we have two `exchange`s that are run concurrently:
;
; - (1) Exchange `account1` and `account2`:
;   - (1.1) Lock `account1`
;   - (1.2) Lock `account2`
;   - (1.3) Do the `exchange`-ing
;
; - (2) Exchange `account2` and `account1`:
;   - (2.1) Lock `account2`
;   - (2.2) Lock `account1`
;   - (2.3) Do the `exchange`-ing
;
; There is a case where (1.1) is run, and (2.1) are run simultaneously, then
; (1.2) must wait for (2.1), and (2.2) must wait for (1.1), which becomes a
; deadlock.
;
; The author suggests us to:
;
; - Give each account a unique identification number accound
; - Rewrite `serialized-exchange` so that a process wil always attempt to enter
;   a procedure protecting the lowest-number account first.

#lang sicp
