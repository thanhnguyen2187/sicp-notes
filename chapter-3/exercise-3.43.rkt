; Exercise 3.43
;
; Suppose that the balances in three accounts start out as $10, $20, and $30,
; and that multiple processes run, exchanging the balances in the accounts.
;
; Argue that if the processes are run sequentially, after any number of
; con-current exchanges, the account balances should be $10, $20, and $30 in
; some order.
;
; Draw a timing diagram like the one in Figure 3.29 to show how this condition
; can be violated if the exchanges are implemented using the first version of
; the account-exchange program in this section.
;
; On the other hand, argue that even with this `exchanges` program, the sum of
; the balances in the accounts will be preserved.
;
; Draw a timing diagram to show how even this condition would be violated if we
; did not serialize the transactions on individual accounts.
;
; ---
;
; ; A1: $10
; ; A2: $20
; ; A3: $30
;
; (exchange A1 A2) (exchange A2 A3)
;
; ; A1: $20
; ; A2: $30
; ; A3: $10
;
; We have these steps for an individual exchanging:
;
; 1. Access `account1`'s balance
; 2. Access `account2`'s balance
; 3. Calculate `difference`
; 4. Withdraw `account1`
; 5. Deposit `account2`
;
; If the exchanges are run sequentially, the steps above are not interrupted.
; After any number of exchanges, the values only change order.
;
; If the exchanges are not run sequentially, but each step is "atomic", the sum
; of the balances in the accounts will be preserved, since step 3, 4, 5 are
; guaranteed to create an equilibrium sum.
;
; If step 4 and 5 are not atomic, the equilibrium sum will not be achieved.

#lang sicp

(define (exchange account1 account2)
  ; This procedure works well when only a single process is trying to do the
  ; exchange. Suppose, however, that Peter and Paul both have access to accounts
  ; `a1`, `a2`, and `a3`, and that Peter exchanges `a1` and `a2` while Paul
  ; concurrently exchanges `a1` and `a3`. Even with account deposits and
  ; withdrawals serialized for individual accounts (as in the `make-account`
  ; procedure shown above in this section), `exchange` can still procedure
  ; incorrect results.
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               amount)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (deposit account amount)
  (let ((s (acount 'serializer))
        (d (acount 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange)) account1 account2)))



